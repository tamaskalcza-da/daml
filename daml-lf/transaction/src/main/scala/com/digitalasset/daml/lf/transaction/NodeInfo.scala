// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.transaction

/** Trait for extracting information from an abstract node.
  * Used for sharing the implementation of common computations
  * over nodes and transactions.
  *
  * External codebases use these utilities on transaction and
  * node implementations that are not the one defined by [[Node]]
  * and hence the need for the indirection.
  */
trait NodeInfo[PartyRep] extends Product with Serializable {

  /** Compute the informees of a node based on the ledger model definition.
    *
    * Refer to https://docs.daml.com/concepts/ledger-model/ledger-privacy.html#projections
    */
  def informeesOfNode: Set[PartyRep]

  /** Required authorizers (see ledger model); UNSAFE TO USE on fetch nodes of transaction with versions < 5
    *
    * The ledger model defines the fetch node actingParties as the nodes' required authorizers.
    * However, the our transaction data structure did not include the actingParties in versions < 5.
    * The usage of this method must thus be restricted to:
    * 1. settings where no fetch nodes appear (for example, the `validate` method of DAMLe, which uses it on root
    *    nodes, which are guaranteed never to contain a fetch node)
    * 2. DAML ledger implementations that do not store or process any transactions with version < 5
    *
    */
  def requiredAuthorizers: Set[PartyRep]
}

object NodeInfo {

  trait Create[PartyRep] extends NodeInfo[PartyRep] {
    def signatories: Set[PartyRep]
    def stakeholders: Set[PartyRep]

    final def requiredAuthorizers: Set[PartyRep] = signatories
    final def informeesOfNode: Set[PartyRep] = stakeholders
  }

  trait Fetch[PartyRep] extends NodeInfo[PartyRep] {
    def signatories: Set[PartyRep]
    def stakeholders: Set[PartyRep]
    def actingParties: Option[Set[PartyRep]]

    // FIXME(JM): comment about tx version < 5

    final def requiredAuthorizers: Set[PartyRep] = actingParties.get
    final def informeesOfNode: Set[PartyRep] = signatories | actingParties.get
  }

  trait Exercise[PartyRep] extends NodeInfo[PartyRep] {

    def consuming: Boolean
    def signatories: Set[PartyRep]
    def stakeholders: Set[PartyRep]
    def actingParties: Set[PartyRep]

    final def requiredAuthorizers(): Set[PartyRep] = actingParties

    final def informeesOfNode: Set[PartyRep] =
      if (consuming)
        stakeholders | actingParties
      else
        signatories | actingParties
  }

  trait LookupByKey[PartyRep] extends NodeInfo[PartyRep] {
    def keyMaintainers: Set[PartyRep]
    def hasResult: Boolean

    final def requiredAuthorizers(): Set[PartyRep] = keyMaintainers
    final def informeesOfNode: Set[PartyRep] =
      // TODO(JM): In the successful case the informees should be the
      // signatories of the fetch contract. The signatories should be
      // added to the LookupByKey node, or a successful lookup should
      // become a Fetch.
      keyMaintainers
  }
}
