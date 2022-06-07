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

// use super::{ActiveSession, AtStake, BalanceOf, Config, Pallet};
use super::{Config, Pallet};
use crate::set::OrderedSet;
use codec::{Codec, Decode, Encode, HasCompact, MaxEncodedLen};
use frame_support::{
    bounded_vec, pallet_prelude::Get, traits::VoteTally, BoundedVec, CloneNoBound, DefaultNoBound, EqNoBound,
    PartialEqNoBound, RuntimeDebugNoBound,
};
use sp_runtime::{
    traits::{AtLeast32BitUnsigned, Convert, Saturating, Zero},
    RuntimeDebug,
};
use sp_staking::SessionIndex;
use sp_std::marker::PhantomData;
use sp_std::{cmp::Ordering, convert::From, prelude::*};

/// The index of a slashing span - unique to each controller.
pub(crate) type SpanIndex = u32;

/// The type define for validators reward
pub(crate) type RewardPoint = u32;

#[derive(Clone, Encode, Decode, RuntimeDebug, MaxEncodedLen, scale_info::TypeInfo)]
pub struct Bond<AccountId, Balance> {
    pub owner: AccountId,
    pub amount: Balance,
}

impl<A, B: Default> Bond<A, B> {
    pub(crate) fn from_owner(owner: A) -> Self {
        Bond {
            owner,
            amount: B::default(),
        }
    }
}

impl<AccountId: Ord, Balance> Eq for Bond<AccountId, Balance> {}

impl<AccountId: Ord, Balance> PartialEq for Bond<AccountId, Balance> {
    fn eq(&self, other: &Self) -> bool {
        self.owner == other.owner
    }
}

impl<AccountId: Ord, Balance> Ord for Bond<AccountId, Balance> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.owner.cmp(&other.owner)
    }
}

impl<AccountId: Ord, Balance> PartialOrd for Bond<AccountId, Balance> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Just a Balance/BlockNumber tuple to encode when a chunk of funds will be unlocked.
#[derive(PartialEq, Eq, Clone, Encode, Decode, RuntimeDebug, Copy, MaxEncodedLen, scale_info::TypeInfo)]
pub struct UnlockChunk<Balance> {
    /// Amount of funds to be unlocked.
    pub(crate) value: Balance,
    /// Session number at which point it'll be unlocked.
    pub(crate) session_idx: SessionIndex,
}

pub(crate) type StakeReward<Balance> = UnlockChunk<Balance>;

#[derive(Copy, Clone, PartialEq, Eq, Encode, Decode, RuntimeDebug, MaxEncodedLen, scale_info::TypeInfo)]
/// The activity status of the validator
pub enum ValidatorStatus {
    /// Committed to be online and producing valid blocks
    Active,
    /// Temporarily inactive
    Idle,
    /// Bonded until the inner round
    Leaving(SessionIndex),
}

impl Default for ValidatorStatus {
    fn default() -> ValidatorStatus {
        ValidatorStatus::Active
    }
}

/// Global validator state with commission fee, bonded stake, and nominations
#[derive(Encode, Decode, Default, Clone, RuntimeDebug, MaxEncodedLen, scale_info::TypeInfo)]
#[scale_info(skip_type_params(MaxNominators, MaxUnlock))]
pub struct Validator<AccountId, Balance, MaxNominators, MaxUnlock>
where
    MaxNominators: Get<u32>,
    MaxUnlock: Get<u32>,
{
    pub id: AccountId,
    pub bond: Balance,
    pub nomi_bond_total: Balance,
    pub nominators: OrderedSet<Bond<AccountId, Balance>, MaxNominators>,
    pub total: Balance,
    pub state: ValidatorStatus,
    pub unlocking: BoundedVec<UnlockChunk<Balance>, MaxUnlock>,
}

impl<AccountId, Balance, MaxNominators, MaxUnlock> Validator<AccountId, Balance, MaxNominators, MaxUnlock>
where
    AccountId: Ord + Clone + sp_std::fmt::Debug,
    Balance: AtLeast32BitUnsigned + Ord + Copy + sp_std::ops::AddAssign + sp_std::ops::SubAssign + Default,
    MaxNominators: Get<u32>,
    MaxUnlock: Get<u32>,
{
    pub fn new(id: AccountId, bond: Balance) -> Self {
        let total = bond;
        let unlocking: BoundedVec<UnlockChunk<Balance>, MaxUnlock> = bounded_vec![];
        Validator {
            id,
            bond,
            nomi_bond_total: Zero::zero(),
            nominators: OrderedSet::<Bond<AccountId, Balance>, MaxNominators>::new(),
            total,
            state: ValidatorStatus::default(), // default active
            unlocking: unlocking,
        }
    }
    pub fn is_active(&self) -> bool {
        self.state == ValidatorStatus::Active
    }
    pub fn is_leaving(&self) -> bool {
        matches!(self.state, ValidatorStatus::Leaving(_))
    }
    pub fn bond_more(&mut self, more: Balance) {
        self.bond = self.bond.saturating_add(more);
        self.total = self.total.saturating_add(more);
    }
    // Returns None if underflow or less == self.bond (in which case validator should leave)
    pub fn bond_less(&mut self, less: Balance) -> Option<Balance> {
        if self.bond > less {
            self.bond = self.bond.saturating_sub(less);
            Some(self.bond)
        } else {
            None
        }
    }
    pub fn inc_nominator(&mut self, nominator: AccountId, more: Balance) {
        if let Ok(loc) = self.nominators.0.binary_search(&Bond::from_owner(nominator)) {
            let nom_bond = &mut self.nominators.0[loc];
            nom_bond.amount = nom_bond.amount.saturating_add(more);
            self.nomi_bond_total = self.nomi_bond_total.saturating_add(more);
            self.total = self.total.saturating_add(more);
        };
    }
    pub fn dec_nominator(&mut self, nominator: AccountId, less: Balance) {
        if let Ok(loc) = self.nominators.0.binary_search(&Bond::from_owner(nominator)) {
            let nom_bond = &mut self.nominators.0[loc];
            nom_bond.amount = nom_bond.amount.saturating_sub(less);
            self.nomi_bond_total = self.nomi_bond_total.saturating_sub(less);
            self.total = self.total.saturating_sub(less);
        };
    }
    pub fn go_offline(&mut self) {
        self.state = ValidatorStatus::Idle;
    }
    pub fn go_online(&mut self) {
        self.state = ValidatorStatus::Active;
    }
    pub fn leave_validators_pool(&mut self, round: SessionIndex) {
        self.state = ValidatorStatus::Leaving(round);
    }
}

impl<AccountId, Balance, MaxNominators, MaxUnlock> Validator<AccountId, Balance, MaxNominators, MaxUnlock>
where
    Balance: AtLeast32BitUnsigned + Saturating + Copy + sp_std::ops::AddAssign + sp_std::ops::SubAssign,
    MaxNominators: Get<u32>,
    MaxUnlock: Get<u32>,
{
    /// Slash the validator for a given amount of balance. This can grow the value
    /// of the slash in the case that the validator has less than `minimum_balance`
    /// active funds. Returns the amount of funds actually slashed.
    ///
    /// Slashes from `active` funds first, and then `unlocking`, starting with the
    /// chunks that are closest to unlocking.
    pub(crate) fn slash(&mut self, mut value: Balance, minimum_balance: Balance) -> Balance {
        let pre_total = self.total;
        let total = &mut self.total;
        let active = &mut self.bond;

        let slash_out_of = |total_remaining: &mut Balance, target: &mut Balance, value: &mut Balance| {
            let mut slash_from_target = (*value).min(*target);

            if !slash_from_target.is_zero() {
                *target = target.saturating_sub(slash_from_target);

                // Make sure not drop below ED
                if *target <= minimum_balance {
                    let diff_val = minimum_balance.saturating_sub(*target);
                    *target = target.saturating_add(diff_val);
                    slash_from_target = slash_from_target.saturating_sub(diff_val);
                }
                *total_remaining = total_remaining.saturating_sub(slash_from_target);
                *value = value.saturating_sub(slash_from_target);
            }
        };

        slash_out_of(total, active, &mut value);

        let i = self
            .unlocking
            .iter_mut()
            .map(|chunk| {
                slash_out_of(total, &mut chunk.value, &mut value);
                chunk.value
            })
            .take_while(|value| value.is_zero()) // take all fully-consumed chunks out.
            .count();

        // kill all drained chunks.
        let _ = self.unlocking.drain(..i);

        pre_total.saturating_sub(*total)
    }
    /// Remove entries from `unlocking` that are sufficiently old and reduce the
    /// total by the sum of their balances.
    pub fn consolidate_unlocked(&mut self, current_session: SessionIndex) -> Balance {
        let mut total = self.total;
        self.unlocking.retain(|&chunk| {
            if chunk.session_idx > current_session {
                true
            } else {
                total = total.saturating_sub(chunk.value);
                false
            }
        });
        let unlocked_val = self.total.saturating_sub(total);
        self.total = total;
        unlocked_val
    }
}

/// Information concerning the direct vote-casting of some voting power.
#[derive(Encode, Decode, Clone, Eq, PartialEq, RuntimeDebug, scale_info::TypeInfo, MaxEncodedLen)]
#[scale_info(skip_type_params(MaxVotes))]
pub struct Casting<Balance, BlockNumber, PollIndex, MaxVotes>
where
    MaxVotes: Get<u32>,
{
    /// The current votes of the account.
    // pub votes: BoundedVec<(PollIndex, AccountVote<Balance>), MaxVotes>,
    pub votes: BoundedVec<(PollIndex, Balance), MaxVotes>,

    pub dummy1: BoundedVec<(PollIndex, BlockNumber), MaxVotes>,
    // /// The total amount of delegations that this account has received, post-conviction-weighting.
    // pub delegations: Delegations<Balance>,
    // /// Any pre-existing locks from past voting/delegating activity.
    // pub prior: PriorLock<BlockNumber, Balance>,
}

/// An indicator for what an account is doing; it can either be delegating or voting.
#[derive(Encode, Decode, Clone, Eq, PartialEq, RuntimeDebug, scale_info::TypeInfo, MaxEncodedLen)]
#[scale_info(skip_type_params(MaxVotes))]
pub enum Voting<Balance, AccountId, BlockNumber, PollIndex, MaxVotes>
where
    MaxVotes: Get<u32>,
{
    /// The account is voting directly.
    Casting(Casting<Balance, BlockNumber, PollIndex, MaxVotes>),
    // /// The account is delegating `balance` of its balance to a `target` account with `conviction`.
    // Delegating(Delegating<Balance, AccountId, BlockNumber>),
    Dummy(AccountId),
}

impl<Balance: Default, AccountId, BlockNumber: Zero, PollIndex, MaxVotes> Default
    for Voting<Balance, AccountId, BlockNumber, PollIndex, MaxVotes>
where
    MaxVotes: Get<u32>,
{
    fn default() -> Self {
        Voting::Casting(Casting {
            votes: Default::default(),
            dummy1: Default::default(),
            // delegations: Default::default(),
            // prior: PriorLock(Zero::zero(), Default::default()),
        })
    }
}

/// Info regarding an ongoing referendum.
#[derive(
    CloneNoBound,
    DefaultNoBound,
    PartialEqNoBound,
    EqNoBound,
    RuntimeDebugNoBound,
    scale_info::TypeInfo,
    Encode,
    Decode,
    MaxEncodedLen,
)]
#[scale_info(skip_type_params(Total))]
pub struct Tally<Votes: Clone + Default + PartialEq + Eq + sp_std::fmt::Debug + scale_info::TypeInfo + Codec, Total> {
    /// The number of aye votes, expressed in terms of post-conviction lock-vote.
    pub ayes: Votes,
    /// The number of nay votes, expressed in terms of post-conviction lock-vote.
    pub nays: Votes,
    /// The amount of funds currently expressing its opinion. Pre-conviction.
    pub turnout: Votes,
    /// Dummy.
    dummy: PhantomData<Total>,
}

impl<
        Votes: Clone
            + Default
            + PartialEq
            + Eq
            + sp_std::fmt::Debug
            + Copy
            + AtLeast32BitUnsigned
            + scale_info::TypeInfo
            + Codec,
        Total: Get<Votes>,
    > Tally<Votes, Total>
{
    pub fn from_parts(ayes: Votes, nays: Votes, turnout: Votes) -> Self {
        Self {
            ayes,
            nays,
            turnout,
            dummy: PhantomData,
        }
    }
}
