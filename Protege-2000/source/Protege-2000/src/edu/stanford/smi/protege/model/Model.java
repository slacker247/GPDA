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
 * Standard Class, Slot and Facet names
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Model {

    /**
     *  standard class names
     *
     * @author Ray Fergerson <fergerson@smi.stanford.edu>
     */
    public interface Cls {

        String THING = ":THING";
        String CLASS = ":CLASS";
        String STANDARD_CLASS = ":STANDARD-CLASS";
        String SLOT = ":SLOT";
        String STANDARD_SLOT = ":STANDARD-SLOT";
        String FACET = ":FACET";
        String STANDARD_FACET = ":STANDARD-FACET";

        String INDIVIDUAL = ":INDIVIDUAL";
        String NUMBER = ":NUMBER";
        String INTEGER = ":INTEGER";
        String FLOAT = ":FLOAT";
        String STRING = ":STRING";
        String SYMBOL = ":SYMBOL";
        String BOOLEAN = ":BOOLEAN";

        String PRIMITIVE_TYPE = ":PRIMITIVE-TYPE";
        String SYSTEM_CLASS = ":SYSTEM-CLASS";
        String CONSTRAINT = ":CONSTRAINT";
        String RELATION = ":RELATION";

        String PAL_CONSTRAINT = ":PAL-CONSTRAINT";

        String ANNOTATION = ":ANNOTATION";
        String INSTANCE_ANNOTATION = ":INSTANCE-ANNOTATION";

        /**
         *  Description of the Class
         *
         * @author Ray Fergerson <fergerson@smi.stanford.edu>
         */
        public interface ID {
            int BASE_ID = 1000;
            FrameID THING = new FrameID(BASE_ID + 0);
            FrameID CLASS = new FrameID(BASE_ID + 1);
            FrameID STANDARD_CLASS = new FrameID(BASE_ID + 2);
            FrameID SLOT = new FrameID(BASE_ID + 3);
            FrameID STANDARD_SLOT = new FrameID(BASE_ID + 4);
            FrameID FACET = new FrameID(BASE_ID + 5);
            FrameID STANDARD_FACET = new FrameID(BASE_ID + 6);

            FrameID INDIVIDUAL = new FrameID(BASE_ID + 7);
            FrameID NUMBER = new FrameID(BASE_ID + 8);
            FrameID INTEGER = new FrameID(BASE_ID + 9);
            FrameID FLOAT = new FrameID(BASE_ID + 10);
            FrameID STRING = new FrameID(BASE_ID + 11);
            FrameID SYMBOL = new FrameID(BASE_ID + 12);
            FrameID BOOLEAN = new FrameID(BASE_ID + 13);

            FrameID PRIMITIVE_TYPE = new FrameID(BASE_ID + 14);
            FrameID SYSTEM_CLASS = new FrameID(BASE_ID + 15);
            FrameID CONSTRAINT = new FrameID(BASE_ID + 16);
            FrameID RELATION = new FrameID(BASE_ID + 17);

            FrameID PAL_CONSTRAINT = new FrameID(BASE_ID + 18);

            FrameID ANNOTATION = new FrameID(BASE_ID + 19);
            FrameID INSTANCE_ANNOTATION = new FrameID(BASE_ID + 20);
        }

    }

    /**
     * standard slot names
     *
     * @author Ray Fergerson <fergerson@smi.stanford.edu>
     */
    public interface Slot {
        String DOCUMENTATION = ":DOCUMENTATION";
        String DOMAIN = ":DOMAIN";

        String NAME = ":NAME";
        String ROLE = ":ROLE";
        String DIRECT_SUPERCLASSES = ":DIRECT-SUPERCLASSES";
        String DIRECT_SUBCLASSES = ":DIRECT-SUBCLASSES";
        String DIRECT_TYPE = ":DIRECT-TYPE";
        String DIRECT_INSTANCES = ":DIRECT-INSTANCES";
        String DIRECT_TEMPLATE_SLOTS = ":DIRECT-TEMPLATE-SLOTS";
        // String DIRECT_BROWSER_SLOT = ":DIRECT-BROWSER-SLOT";

        // String OWN_SLOTS = ":OWN-SLOTS";
        String ASSOCIATED_FACET = ":ASSOCIATED-FACET";

        String CONSTRAINTS = ":SLOT-CONSTRAINTS";
        String DEFAULTS = ":SLOT-DEFAULTS";
        String VALUE_TYPE = ":SLOT-VALUE-TYPE";
        String INVERSE = ":SLOT-INVERSE";
        String CARDINALITY = ":SLOT-CARDINALITY";
        String MAXIMUM_CARDINALITY = ":SLOT-MAXIMUM-CARDINALITY";
        String MINIMUM_CARDINALITY = ":SLOT-MINIMUM-CARDINALITY";
        String SAME_VALUES = ":SLOT-SAME-VALUES";
        String NOT_SAME_VALUES = ":SLOT-NOT-SAME-VALUES";
        String SUBSET_OF_VALUES = ":SLOT-SUBSET-OF-VALUES";
        String NUMERIC_MINIMUM = ":SLOT-NUMERIC-MINIMUM";
        String NUMERIC_MAXIMUM = ":SLOT-NUMERIC-MAXIMUM";
        String SOME_VALUES = ":SLOT-SOME-VALUES";
        String COLLECTION_TYPE = ":SLOT-COLLECTION-TYPE";

        String PAL_STATEMENT = ":PAL-STATEMENT";
        String PAL_DESCRIPTION = ":PAL-DESCRIPTION";
        String PAL_NAME = ":PAL-NAME";
        String PAL_RANGE = ":PAL-RANGE";

        String VALUES = ":SLOT-VALUES";

        String DIRECT_SUBSLOTS = ":DIRECT-SUBSLOTS";
        String DIRECT_SUPERSLOTS = ":DIRECT-SUPERSLOTS";

        String ANNOTATED_INSTANCE = ":ANNOTATED-INSTANCE";
        String ANNOTATION_TEXT = ":ANNOTATION-TEXT";
        String CREATOR = ":CREATOR";
        String CREATION_TIMESTAMP = ":CREATION-TIMESTAMP";

        String ASSOCIATED_SLOT = ":ASSOCIATED-SLOT";
        String MODIFIER = ":MODIFIER";
        String MODIFICATION_TIMESTAMP = ":MODIFICATION-TIMESTAMP";

        /**
         *  FrameID's for slots
         *
         * @author Ray Fergerson <fergerson@smi.stanford.edu>
         */
        public interface ID {
            int BASE_ID = 2000;
            FrameID DOCUMENTATION = new FrameID(BASE_ID + 0);
            FrameID DOMAIN = new FrameID(BASE_ID + 1);

            FrameID NAME = new FrameID(BASE_ID + 2);
            FrameID ROLE = new FrameID(BASE_ID + 3);
            FrameID DIRECT_SUPERCLASSES = new FrameID(BASE_ID + 4);
            FrameID DIRECT_SUBCLASSES = new FrameID(BASE_ID + 5);
            FrameID DIRECT_TYPE = new FrameID(BASE_ID + 6);
            FrameID DIRECT_INSTANCES = new FrameID(BASE_ID + 7);
            FrameID DIRECT_TEMPLATE_SLOTS = new FrameID(BASE_ID + 8);
            // FrameID DIRECT_BROWSER_SLOT = new FrameID(BASE_ID + 9);

            // FrameID OWN_SLOTS = new FrameID(BASE_ID + 10);
            FrameID ASSOCIATED_FACET = new FrameID(BASE_ID + 11);

            FrameID CONSTRAINTS = new FrameID(BASE_ID + 12);
            FrameID DEFAULTS = new FrameID(BASE_ID + 13);
            FrameID VALUE_TYPE = new FrameID(BASE_ID + 14);
            FrameID INVERSE = new FrameID(BASE_ID + 15);
            FrameID CARDINALITY = new FrameID(BASE_ID + 16);
            FrameID MAXIMUM_CARDINALITY = new FrameID(BASE_ID + 17);
            FrameID MINIMUM_CARDINALITY = new FrameID(BASE_ID + 18);
            FrameID SAME_VALUES = new FrameID(BASE_ID + 19);
            FrameID NOT_SAME_VALUES = new FrameID(BASE_ID + 20);
            FrameID SUBSET_OF_VALUES = new FrameID(BASE_ID + 21);
            FrameID NUMERIC_MINIMUM = new FrameID(BASE_ID + 22);
            FrameID NUMERIC_MAXIMUM = new FrameID(BASE_ID + 23);
            FrameID SOME_VALUES = new FrameID(BASE_ID + 24);
            FrameID COLLECTION_TYPE = new FrameID(BASE_ID + 25);

            FrameID PAL_STATEMENT = new FrameID(BASE_ID + 26);
            FrameID PAL_DESCRIPTION = new FrameID(BASE_ID + 27);
            FrameID PAL_NAME = new FrameID(BASE_ID + 28);
            FrameID PAL_RANGE = new FrameID(BASE_ID + 29);

            FrameID VALUES = new FrameID(BASE_ID + 30);

            FrameID DIRECT_SUBSLOTS = new FrameID(BASE_ID + 31);
            FrameID DIRECT_SUPERSLOTS = new FrameID(BASE_ID + 32);

            FrameID ANNOTATED_INSTANCE = new FrameID(BASE_ID + 33);
            FrameID ANNOTATION_TEXT = new FrameID(BASE_ID + 34);
            FrameID CREATOR = new FrameID(BASE_ID + 36);
            FrameID CREATION_TIMESTAMP = new FrameID(BASE_ID + 37);
            FrameID ASSOCIATED_SLOT = new FrameID(BASE_ID + 38);
            FrameID MODIFIER = new FrameID(BASE_ID + 39);
            FrameID MODIFICATION_TIMESTAMP = new FrameID(BASE_ID + 40);
        }
    }

    /**
     * standard facet names
     *
     * @author Ray Fergerson <fergerson@smi.stanford.edu>
     */
    public interface Facet {
        String DIRECT_TEMPLATE_FACETS = ":DIRECT-TEMPLATE-FACETS";

        String DOCUMENTATION = ":DOCUMENTATION-IN-FRAME";
        String DEFAULTS = ":DEFAULTS";
        String CONSTRAINTS = ":CONSTRAINTS";

        String VALUE_TYPE = ":VALUE-TYPE";
        String INVERSE = ":INVERSE";
        String CARDINALITY = ":CARDINALITY";
        String MAXIMUM_CARDINALITY = ":MAXIMUM-CARDINALITY";
        String MINIMUM_CARDINALITY = ":MINIMUM-CARDINALITY";
        String SAME_VALUES = ":SAME-VALUES";
        String NOT_SAME_VALUES = ":NOT-SAME-VALUES";
        String SUBSET_OF_VALUES = ":SUBSET-OF-VALUES";
        String NUMERIC_MINIMUM = ":NUMERIC-MINIMUM";
        String NUMERIC_MAXIMUM = ":NUMERIC-MAXIMUM";
        String SOME_VALUES = ":SOME-VALUES";
        String COLLECTION_TYPE = ":COLLECTION-TYPE";

        String VALUES = ":VALUES";
        String MODIFIER = ":MODIFIER-FACET";
        String MODIFICATION_TIMESTAMP = ":MODIFICATION-TIMESTAMP-FACET";

        /**
         *  Description of the Class
         *
         * @author Ray Fergerson <fergerson@smi.stanford.edu>
         */
        public interface ID {
            int BASE_ID = 3000;
            FrameID DIRECT_TEMPLATE_FACETS = new FrameID(BASE_ID + 0);

            FrameID DOCUMENTATION = new FrameID(BASE_ID + 1);
            FrameID DEFAULTS = new FrameID(BASE_ID + 2);
            FrameID CONSTRAINTS = new FrameID(BASE_ID + 3);

            FrameID VALUE_TYPE = new FrameID(BASE_ID + 4);
            FrameID INVERSE = new FrameID(BASE_ID + 5);
            FrameID CARDINALITY = new FrameID(BASE_ID + 6);
            FrameID MAXIMUM_CARDINALITY = new FrameID(BASE_ID + 7);
            FrameID MINIMUM_CARDINALITY = new FrameID(BASE_ID + 8);
            FrameID SAME_VALUES = new FrameID(BASE_ID + 9);
            FrameID NOT_SAME_VALUES = new FrameID(BASE_ID + 10);
            FrameID SUBSET_OF_VALUES = new FrameID(BASE_ID + 11);
            FrameID NUMERIC_MINIMUM = new FrameID(BASE_ID + 12);
            FrameID NUMERIC_MAXIMUM = new FrameID(BASE_ID + 13);
            FrameID SOME_VALUES = new FrameID(BASE_ID + 14);
            FrameID COLLECTION_TYPE = new FrameID(BASE_ID + 15);

            FrameID VALUES = new FrameID(BASE_ID + 16);

            FrameID MODIFIER = new FrameID(BASE_ID + 17);
            FrameID MODIFICATION_TIMESTAMP = new FrameID(BASE_ID + 18);
        }
    }

}
