/*
 * This file is part of the Nodle Chain distributed at https://github.com/NodleCode/chain
 * Copyright (C) 2020-2022  Nodle International
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

//! Nodle Chain - custom tailored, staking pallet.
//! Use a non inflationary reward system.

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(test)]
mod mock;

use frame_support::pallet;
mod set;
mod types;

pub use pallet::*;

#[pallet]
pub mod pallet {
    use super::*;
    use frame_support::{
        bounded_vec,
        pallet_prelude::*,
        traits::{
            Currency, ExistenceRequirement, Get, Imbalance, LockIdentifier, LockableCurrency, OnUnbalanced, Polling,
            ValidatorRegistration, WithdrawReasons,
        },
        BoundedVec, PalletId,
    };
    use frame_system::pallet_prelude::*;
    use set::OrderedSet;
    use types::{Bond, Tally, Validator, Voting};

    pub(crate) type BalanceOf<T, I = ()> =
        <<T as Config<I>>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

    pub(crate) type VotingOf<T, I = ()> = Voting<
        BalanceOf<T, I>,
        <T as frame_system::Config>::AccountId,
        <T as frame_system::Config>::BlockNumber,
        PollIndexOf<T, I>,
        <T as Config<I>>::MaxVotes,
    >;
    pub(crate) type TallyOf<T, I = ()> = Tally<BalanceOf<T, I>, <T as Config<I>>::MaxTurnout>;
    pub(crate) type PollIndexOf<T, I = ()> = <<T as Config<I>>::Polls as Polling<TallyOf<T, I>>>::Index;

    #[pallet::config]
    pub trait Config<I: 'static = ()>: frame_system::Config + Sized {
        // System level stuff.
        type Event: From<Event<Self, I>> + IsType<<Self as frame_system::Config>::Event>;
        /// The staking balance.
        type Currency: LockableCurrency<Self::AccountId, Moment = Self::BlockNumber>;
        /// Maximum nominators per validator
        #[pallet::constant]
        type MaxNominatorsPerValidator: Get<u32>;
        /// Maximum validators allowed to join the pool.
        #[pallet::constant]
        type DefaultStakingMaxValidators: Get<u32>;
        /// Max number of unbond request supported by queue
        #[pallet::constant]
        type MaxChunkUnlock: Get<u32>;

        /// The implementation of the logic which conducts polls.
        type Polls: Polling<TallyOf<Self, I>, Votes = BalanceOf<Self, I>, Moment = Self::BlockNumber>;

        /// The maximum amount of tokens which may be used for voting. May just be
        /// `Currency::total_issuance`, but you might want to reduce this in order to account for
        /// funds in the system which are unable to vote (e.g. parachain auction deposits).
        type MaxTurnout: Get<BalanceOf<Self, I>>;
        /// The maximum number of concurrent votes an account may have.
        ///
        /// Also used to compute weight, an overly large value can
        /// lead to extrinsic with large weight estimation: see `delegate` for instance.
        #[pallet::constant]
        type MaxVotes: Get<u32>;
        // /// staking pallet Lock Identifier used for set_lock()
        // #[pallet::constant]
        // type StakingLockId: Get<LockIdentifier>;
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub(crate) trait Store)]
    #[pallet::without_storage_info]
    pub struct Pallet<T, I = ()>(_);

    // pub struct Pallet<T>(PhantomData<T>);

    // #[pallet::hooks]
    // impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {}

    #[pallet::call]
    impl<T: Config<I>, I: 'static> Pallet<T, I> {
        #[pallet::weight(1000)]
        pub fn validator_join_pool(origin: OriginFor<T>, bond: BalanceOf<T, I>) -> DispatchResultWithPostInfo {
            log::debug!("validator_join_pool:[{:#?}] - Entry!!!", line!(),);

            let acc = ensure_signed(origin)?;

            log::debug!("validator_join_pool:[{:#?}]", line!(),);

            let mut validators = <ValidatorPool<T, I>>::get();

            ensure!(
                validators.insert(Bond {
                    owner: acc.clone(),
                    amount: bond
                }),
                <Error<T, I>>::ValidatorExists
            );
            log::debug!("validator_join_pool:[{:#?}]", line!());

            let validator_free_balance = T::Currency::free_balance(&acc);
            log::debug!(
                "validator_join_pool:[{:#?}] | acc::[{:#?}] | bond::[{:#?}] | free_bal:[{:#?}]",
                line!(),
                acc,
                bond,
                validator_free_balance
            );
            ensure!(validator_free_balance >= bond, <Error<T, I>>::InsufficientBalance);

            log::debug!("validator_join_pool:[{:#?}]", line!(),);

            // T::Currency::set_lock(T::StakingLockId::get(), &acc, bond, WithdrawReasons::all());

            let validator = Validator::new(acc.clone(), bond);

            // <Total<T>>::mutate(|x| *x = x.saturating_add(bond));
            <ValidatorState<T, I>>::insert(&acc, validator);
            // <ValidatorState<T, I>>::insert(&acc, validator.clone());
            // <ValidatorPool<T>>::put(validators);
            // Self::deposit_event(Event::JoinedValidatorPool(acc, bond, Self::total()));
            log::debug!("validator_join_pool:[{:#?}] - Exit!!!", line!(),);
            Ok(().into())
        }
    }

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config<I>, I: 'static = ()> {}

    #[pallet::error]
    pub enum Error<T, I = ()> {
        /// Validator account already part of validator pool.
        ValidatorExists,
        /// Low free balance in caller account.
        InsufficientBalance,
    }

    /// Get validator state associated with an account if account is collating else None
    #[pallet::storage]
    #[pallet::getter(fn validator_state)]
    pub(crate) type ValidatorState<T: Config<I>, I: 'static = ()> = StorageMap<
        _,
        Twox64Concat,
        T::AccountId,
        Validator<T::AccountId, BalanceOf<T, I>, T::MaxNominatorsPerValidator, T::MaxChunkUnlock>,
        OptionQuery,
    >;

    /// The pool of validator validators, each with their total backing stake
    #[pallet::storage]
    #[pallet::getter(fn validator_pool)]
    pub(crate) type ValidatorPool<T: Config<I>, I: 'static = ()> =
        StorageValue<_, OrderedSet<Bond<T::AccountId, BalanceOf<T, I>>, T::DefaultStakingMaxValidators>, ValueQuery>;

    /// All voting for a particular voter in a particular voting class. We store the balance for the
    /// number of votes that we have recorded.
    #[pallet::storage]
    pub type VotingFor<T: Config<I>, I: 'static = ()> =
        StorageMap<_, Twox64Concat, T::AccountId, VotingOf<T, I>, ValueQuery>;

    impl<T: Config> Pallet<T> {}
}
