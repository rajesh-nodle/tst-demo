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

use std::collections::BTreeMap;

use frame_support::{
    assert_noop, assert_ok,
    pallet_prelude::*,
    parameter_types,
    traits::{ConstU32, ConstU64, Contains, PollStatus, Polling, VoteTally},
    weights::constants::RocksDbWeight,
    PalletId,
};

use crate::types::Tally;
use sp_core::H256;
use sp_runtime::{
    testing::{Header, UintAuthorityId},
    traits::{IdentityLookup, Zero},
    Perbill,
};
use sp_staking::{
    offence::{DisableStrategy, OffenceDetails, OnOffenceHandler},
    SessionIndex,
};

use super::*;
use crate as tst_demo;

/// The AccountId alias in this test module.
pub(crate) type AccountId = u64;
pub(crate) type AccountIndex = u64;
pub(crate) type BlockNumber = u64;
pub(crate) type Balance = u128;

type UncheckedExtrinsic = frame_system::mocking::MockUncheckedExtrinsic<Test>;
type Block = frame_system::mocking::MockBlock<Test>;

frame_support::construct_runtime!(
    pub enum Test where
        Block = Block,
        NodeBlock = Block,
        UncheckedExtrinsic = UncheckedExtrinsic,
    {
        System: frame_system::{Pallet, Call, Config, Storage, Event<T>},
        Balances: pallet_balances::{Pallet, Call, Storage, Config<T>, Event<T>},
        TstDemo: tst_demo::{Pallet, Call, Storage, Event<T>},
    }
);

parameter_types! {
    pub const BlockHashCount: u64 = 250;
    pub BlockWeights: frame_system::limits::BlockWeights =
        frame_system::limits::BlockWeights::simple_max(
            frame_support::weights::constants::WEIGHT_PER_SECOND * 2
        );
    pub const MaxLocks: u32 = 1024;
    // pub static SessionsPerEra: SessionIndex = 3;
    pub static ExistentialDeposit: Balance = 1;
    pub static SlashDeferDuration: SessionIndex = 0;
    pub static BondedDuration: u32 = 2;
    pub static ElectionLookahead: BlockNumber = 0;
    pub static Period: BlockNumber = 5;
    pub static Offset: BlockNumber = 0;
    pub static MaxIterations: u32 = 0;
}
impl frame_system::Config for Test {
    type BlockWeights = ();
    type BlockLength = ();
    type DbWeight = RocksDbWeight;
    type Origin = Origin;
    type Index = AccountIndex;
    type BlockNumber = BlockNumber;
    type Call = Call;
    type Hash = H256;
    type Hashing = ::sp_runtime::traits::BlakeTwo256;
    type AccountId = AccountId;
    type Lookup = IdentityLookup<Self::AccountId>;
    type Header = Header;
    type Event = Event;
    type BlockHashCount = BlockHashCount;
    type Version = ();
    type PalletInfo = PalletInfo;
    type AccountData = pallet_balances::AccountData<Balance>;
    type OnNewAccount = ();
    type OnKilledAccount = ();
    type BaseCallFilter = frame_support::traits::Everything;
    type OnSetCode = ();
    type SystemWeightInfo = ();
    type SS58Prefix = ();
    type MaxConsumers = frame_support::traits::ConstU32<16>;
}
impl pallet_balances::Config for Test {
    type MaxLocks = MaxLocks;
    type Balance = Balance;
    type Event = Event;
    type DustRemoval = ();
    type ExistentialDeposit = ExistentialDeposit;
    type AccountStore = System;
    type MaxReserves = ();
    type ReserveIdentifier = [u8; 8];
    type WeightInfo = ();
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TestPollState {
    Ongoing(TallyOf<Test>, u8),
    Completed(u64, bool),
}
use TestPollState::*;

parameter_types! {
    pub static Polls: BTreeMap<u8, TestPollState> = vec![
        (1, Completed(1, true)),
        (2, Completed(2, false)),
        (3, Ongoing(Tally::from_parts(0, 0, 0), 0)),
    ].into_iter().collect();
}

pub struct TestPolls;
impl Polling<TallyOf<Test>> for TestPolls {
    type Index = u8;
    type Votes = u128;
    type Moment = u64;
    type Class = u8;
    fn classes() -> Vec<u8> {
        vec![0, 1, 2]
    }
    fn as_ongoing(index: u8) -> Option<(TallyOf<Test>, Self::Class)> {
        Polls::get().remove(&index).and_then(|x| {
            if let TestPollState::Ongoing(t, c) = x {
                Some((t, c))
            } else {
                None
            }
        })
    }
    fn access_poll<R>(index: Self::Index, f: impl FnOnce(PollStatus<&mut TallyOf<Test>, u64, u8>) -> R) -> R {
        let mut polls = Polls::get();
        let entry = polls.get_mut(&index);
        let r = match entry {
            Some(Ongoing(ref mut tally_mut_ref, class)) => f(PollStatus::Ongoing(tally_mut_ref, *class)),
            Some(Completed(when, succeeded)) => f(PollStatus::Completed(*when, *succeeded)),
            None => f(PollStatus::None),
        };
        Polls::set(polls);
        r
    }
    fn try_access_poll<R>(
        index: Self::Index,
        f: impl FnOnce(PollStatus<&mut TallyOf<Test>, u64, u8>) -> Result<R, DispatchError>,
    ) -> Result<R, DispatchError> {
        let mut polls = Polls::get();
        let entry = polls.get_mut(&index);
        let r = match entry {
            Some(Ongoing(ref mut tally_mut_ref, class)) => f(PollStatus::Ongoing(tally_mut_ref, *class)),
            Some(Completed(when, succeeded)) => f(PollStatus::Completed(*when, *succeeded)),
            None => f(PollStatus::None),
        }?;
        Polls::set(polls);
        Ok(r)
    }

    #[cfg(feature = "runtime-benchmarks")]
    fn create_ongoing(class: Self::Class) -> Result<Self::Index, ()> {
        let mut polls = Polls::get();
        let i = polls.keys().rev().next().map_or(0, |x| x + 1);
        polls.insert(i, Ongoing(Tally::new(0), class));
        Polls::set(polls);
        Ok(i)
    }

    #[cfg(feature = "runtime-benchmarks")]
    fn end_ongoing(index: Self::Index, approved: bool) -> Result<(), ()> {
        let mut polls = Polls::get();
        match polls.get(&index) {
            Some(Ongoing(..)) => {}
            _ => return Err(()),
        }
        let now = frame_system::Pallet::<Test>::block_number();
        polls.insert(index, Completed(now, approved));
        Polls::set(polls);
        Ok(())
    }
}

impl Config for Test {
    type Event = Event;
    type Currency = Balances;
    type MaxNominatorsPerValidator = ConstU32<4>;
    type DefaultStakingMaxValidators = ConstU32<50>;
    type MaxChunkUnlock = ConstU32<32>;
    type MaxVotes = ConstU32<3>;
    type MaxTurnout = frame_support::traits::TotalIssuanceOf<Balances, Self::AccountId>;
    type Polls = TestPolls;
}
