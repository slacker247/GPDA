/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.protege.model;

/**
 * Encapsulation of method for executing a tranaction
 *
 * Example:
 * <pre> <code>
 * void foo(KnowledgeBase kb) {
 * 		Transaction t = new Transaction(kb) {
 *			public boolean doOperations() {
 *				// kb calls go here
 *				// return true for commit, false for rollback
 *			}
 * 		};
 *		boolean committed = t.execute();
 *		if (committed) {
 *			System.out("transaction committed");
 *		} else {
 * 			System.out("transaction rolled back");
 *		}
 * }
 * </code> </pre>
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class Transaction {
    private DefaultKnowledgeBase _knowledgeBase;

    public Transaction(KnowledgeBase kb) {
        this._knowledgeBase = (DefaultKnowledgeBase) kb;
    }

    /** returns true if the the results of this method should be committed */
    public abstract boolean doOperations();

    /** returns true if the transaction was committed
     *
     * This method handles the logic of executing a transaction, including the
     * necessary synchronization.  In addition, it handles correctly both
     * runtime exceptions and errors.
     */
    public boolean execute() {
        boolean result = false;
        synchronized (_knowledgeBase) {
            boolean transactionComplete = false;
            try {
                _knowledgeBase.beginTransaction();
                boolean doCommit = doOperations();
                result = _knowledgeBase.endTransaction(doCommit);
                transactionComplete = true;
            } finally {
                if (!transactionComplete) {
                    _knowledgeBase.endTransaction(false);
                }
            }
        }
        return result;
    }

    public KnowledgeBase getKnowledgeBase() {
        return _knowledgeBase;
    }
}
