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

package edu.stanford.smi.protege.util;

/**
 *  A utility class that prints trace messages of various sorts to a log. By
 *  default the "log" is the console but it could be directed elsewhere. <p>
 *
 *  The following code is an example of the use of Log.
 *
 * <blockquote><pre>
 * class Foo {
 *     void bar(Object o) {
 *         ...
 *         Log.trace("my message", this, "bar", o);
 *         ...
 *     }
 *     void static baz(Object o1, String s1) {
 *         ...
 *         Log.trace("my message", Foo.class, "baz", o1, s1);
 *         ...
 *     }
 * }
 * </pre></blockquote>
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */

public class Log {
    private static Logger _logger = new DefaultLogger();

    /**
     *  Description of the Class
     *
     * @author Ray Fergerson <fergerson@smi.stanford.edu>
     */
    public interface Logger {
        void setDisplayTrace(boolean b);

        void setDisplayWarnings(boolean b);

        void setDoPause(boolean b);

        void setShowStack(boolean b);

        void enter(Object object, String methodName, Object[] args);

        void exit(Object object, String methodName, Object[] args);

        void trace(String entry, Object object, String methodName, Object[] args);

        void warning(String entry, Object object, String methodName, Object[] args);

        void error(String entry, Object object, String methodName, Object[] args);

        void exception(Throwable e, Object object, String methodName, Object[] args);

        void stack(String entry, Object object, String methodName, Object[] args);

        int getTraceCount();

        int getWarningCount();
        int getErrorCount();
        void resetCounts();
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called (see the {@link Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName) {
        _logger.enter(thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called and passed the listed argument (see the {@link
     *  Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName, Object arg1) {
        _logger.enter(thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called and passed the listed arguments (see the {@link
     *  Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.enter(thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called and passed the listed arguments (see the {@link
     *  Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.enter(thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called and passed the listed arguments (see the {@link
     *  Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4) {
        _logger.enter(thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has been called and passed the listed arguments (see the {@link
     *  Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     * @param  arg5         Description of Parameter
     */
    public static void enter(Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        _logger.enter(thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4, arg5});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName) {
        _logger.error(description, thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName, Object arg1) {
        _logger.error(description, thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.error(description, thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.error(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4) {
        _logger.error(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4});
    }

    /**
     *  Put a message into the log that an error with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     * @param  arg5         Description of Parameter
     */
    public static void error(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        _logger.error(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4, arg5});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> (see the {@link Log Log class
     *  example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> which was called with the listed
     *  arguments (see the {@link Log Log class example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName, Object arg1) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> which was called with the listed
     *  arguments (see the {@link Log Log class example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> which was called with the listed
     *  arguments (see the {@link Log Log class example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> which was called with the listed
     *  arguments (see the {@link Log Log class example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4});
    }

    /**
     *  Put a message into the log that an unexpected exception was caught from
     *  inside of <code>methodName</code> which was called with the listed
     *  arguments (see the {@link Log Log class example}).
     *
     * @param  exception    Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     * @param  arg5         Description of Parameter
     */
    public static void exception(Throwable exception, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        _logger.exception(exception, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4, arg5});
    }

    /**
     *  Make an entry into the log with the message that <code>methodName
     *  </code>has returned (see the {@link Log Log class example}).
     *
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void exit(Object thisOrClass, String methodName) {
        _logger.exit(thisOrClass, methodName, new Object[]{});
    }

    public static int getErrorCount() {
        return _logger.getErrorCount();
    }

    public static int getTraceCount() {
        return _logger.getTraceCount();
    }

    public static int getWarningCount() {
        return _logger.getWarningCount();
    }

    public static void resetCount() {
        _logger.resetCounts();
    }

    public static void resetCounts() {
        _logger.resetCounts();
    }

    public static void setDisplayTrace(boolean b) {
        Log.enter(Log.class, "setDisplayTrace", new Boolean(b));
        _logger.setDisplayTrace(b);
    }

    public static void setDisplayWarnings(boolean b) {
        Log.enter(Log.class, "setDisplayWarnings", new Boolean(b));
        _logger.setDisplayWarnings(b);
    }

    public static void setDoPause(boolean b) {
        Log.enter(Log.class, "setDoPause", new Boolean(b));
        _logger.setDoPause(b);
    }

    public static void setLogger(Logger logger) {
        Log.enter(Log.class, "setLogger", logger);
        _logger = logger;
    }

    public static void setShowStack(boolean b) {
        Log.enter(Log.class, "setShowStack", new Boolean(b));
        _logger.setShowStack(b);
    }

    /**
     *  Put a stack dump and message "description" into the log with the
     *  additional information that the message is occuring from inside of
     *  <code>methodName</code> (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void stack(String description, Object thisOrClass, String methodName) {
        _logger.stack(description, thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Put a stack dump and message "description" into the log with the
     *  additional information that the message is occuring from inside of
     *  <code>methodName</code> which was called with the listed argument (see
     *  the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void stack(String description, Object thisOrClass, String methodName, Object arg1) {
        _logger.stack(description, thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Put a stack dump and message "description" into the log with the
     *  additional information that the message is occuring from inside of
     *  <code>methodName</code> which was called with the listed arguments (see
     *  the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void stack(String description, Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.stack(description, thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Put a stack dump and message "description" into the log with the
     *  additional information that the message is occuring from inside of
     *  <code>methodName</code> which was called with the listed arguments (see
     *  the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void stack(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.stack(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Put a stack dump and message "description" into the log with the
     *  additional information that the message is occuring from inside of
     *  <code>methodName</code> which was called with the listed arguments (see
     *  the {@link Log Log class example}).
     */
    public static void stack(
        String description,
        Object thisOrClass,
        String methodName,
        Object arg1,
        Object arg2,
        Object arg3,
        Object arg4) {
        _logger.stack(description, thisOrClass, methodName, new Object[] { arg1, arg2, arg3, arg4 });
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>(see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>which was called with the listed arguments (see the {@link Log
     *  Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName, Object arg1) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>which was called with the listed arguments (see the {@link Log
     *  Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>which was called with the listed arguments (see the {@link Log
     *  Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>which was called with the listed arguments (see the {@link Log
     *  Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4});
    }

    /**
     *  Put a trace message "description" into the log with the additional
     *  information that the message is occuring from inside of <code>methodName
     *  </code>which was called with the listed arguments (see the {@link Log
     *  Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     * @param  arg5         Description of Parameter
     */
    public static void trace(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        _logger.trace(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4, arg5});
    }

    /**
     *  Put a message into the log that a warning with the given description
     *  occurred from inside of <code>methodName</code> (see the {@link Log Log
     *  class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     */
    public static void warning(String description, Object thisOrClass, String methodName) {
        _logger.warning(description, thisOrClass, methodName, new Object[]{});
    }

    /**
     *  Put a message into the log that a warning with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     */
    public static void warning(String description, Object thisOrClass, String methodName, Object arg1) {
        _logger.warning(description, thisOrClass, methodName, new Object[]{arg1});
    }

    /**
     *  Put a message into the log that a warning with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     */
    public static void warning(String description, Object thisOrClass, String methodName, Object arg1, Object arg2) {
        _logger.warning(description, thisOrClass, methodName, new Object[]{arg1, arg2});
    }

    /**
     *  Put a message into the log that a warning with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     */
    public static void warning(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3) {
        _logger.warning(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3});
    }

    /**
     *  Put a message into the log that a warning with the given description
     *  occurred from inside of <code>methodName</code> which was called with
     *  the listed arguments (see the {@link Log Log class example}).
     *
     * @param  description  Description of Parameter
     * @param  thisOrClass  Description of Parameter
     * @param  methodName   Description of Parameter
     * @param  arg1         Description of Parameter
     * @param  arg2         Description of Parameter
     * @param  arg3         Description of Parameter
     * @param  arg4         Description of Parameter
     */
    public static void warning(String description, Object thisOrClass, String methodName, Object arg1, Object arg2, Object arg3, Object arg4) {
        _logger.warning(description, thisOrClass, methodName, new Object[]{arg1, arg2, arg3, arg4});
    }
}
