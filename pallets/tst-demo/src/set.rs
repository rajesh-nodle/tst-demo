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
use codec::{Decode, Encode, EncodeLike, Input, MaxEncodedLen, Output};
use frame_support::{bounded_vec, pallet_prelude::Get, BoundedVec};
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};
use sp_runtime::RuntimeDebug;
use sp_std::{marker::PhantomData, prelude::*};

/// An ordered set backed by `Vec`
#[derive(Encode, Decode, Clone, Eq, PartialEq, RuntimeDebug, MaxEncodedLen, scale_info::TypeInfo)]
#[scale_info(skip_type_params(S))]
pub struct OrderedSet<T, S: Get<u32>>(pub BoundedVec<T, S>);

impl<T, S: Get<u32>> Default for OrderedSet<T, S> {
    /// Create a default empty set
    fn default() -> Self {
        Self(bounded_vec![])
    }
}

impl<T: Ord + Clone, S: Get<u32>> OrderedSet<T, S> {
    /// Create a new empty set
    pub fn new() -> Self {
        Self(bounded_vec![])
    }

    /// Create a set from a `Vec`.
    /// `v` will be sorted and dedup first.
    pub fn from(mut v: Vec<T>) -> Self {
        v.sort();
        v.dedup();
        Self::from_sorted_set(v)
    }

    /// Create a set from a `Vec`.
    /// Assume `v` is sorted and contain unique elements.
    pub fn from_sorted_set(v: Vec<T>) -> Self {
        match <BoundedVec<T, S>>::try_from(v) {
            Ok(new_set) => Self(new_set),
            Err(_) => panic!("Failed to create ordered set"),
        }
    }

    /// Insert an element.
    /// Return true if insertion happened.
    pub fn insert(&mut self, value: T) -> bool {
        match self.0.to_vec().binary_search(&value) {
            Ok(_) => false,
            Err(loc) => match self.0.try_insert(loc, value) {
                Err(_) => false,
                _ => true,
            },
        }
    }

    /// Remove an element.
    /// Return true if removal happened.
    pub fn remove(&mut self, value: &T) -> bool {
        match self.0.to_vec().binary_search(value) {
            Ok(loc) => {
                self.0.remove(loc);
                true
            }
            Err(_) => false,
        }
    }

    /// Return if the set contains `value`
    pub fn contains(&self, value: &T) -> Option<usize> {
        // self.0.binary_search(&value).is_ok()
        match self.0.to_vec().binary_search(value) {
            Ok(loc) => Some(loc),
            Err(_) => None,
        }
    }

    /// Clear the set
    pub fn clear(&mut self) {
        self.0 = bounded_vec![];
    }
}

impl<T: Ord + Clone, S: Get<u32>> From<Vec<T>> for OrderedSet<T, S> {
    fn from(v: Vec<T>) -> Self {
        Self::from(v)
    }
}
