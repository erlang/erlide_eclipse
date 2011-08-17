/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.model.root;

import org.erlide.core.ErlangCore;

/**
 * Status codes used with Erlang model status objects.
 * <p>
 * This interface declares constants only; it is not intended to be implemented
 * or extended.
 * </p>
 * 
 * @see IErlModelStatus
 * @see org.eclipse.core.runtime.IStatus#getCode()
 */
public final class ErlModelStatusConstants {

    /**
     * Status constant indicating that a classpath entry was invalid
     */
    public static final int INVALID_CODEPATH = 964;

    /**
     * Status constant indicating a core exception occurred. Use
     * <code>getException</code> to retrieve a <code>CoreException</code>.
     */
    public static final int CORE_EXCEPTION = 966;

    /**
     * Status constant indicating one or more of the elements supplied are not
     * of a valid type for the operation to process. The element(s) can be
     * retrieved using <code>getElements</code> on the status object.
     */
    public static final int INVALID_ELEMENT_TYPES = 967;

    /**
     * Status constant indicating that no elements were provided to the
     * operation for processing.
     */
    public static final int NO_ELEMENTS_TO_PROCESS = 968;

    /**
     * Status constant indicating that one or more elements supplied do not
     * exist. The element(s) can be retrieved using <code>getElements</code> on
     * the status object.
     * 
     * @see IErlModelStatus#isDoesNotExist()
     */
    public static final int ELEMENT_DOES_NOT_EXIST = 969;

    /**
     * Status constant indicating that a <code>null</code> path was supplied to
     * the operation.
     */
    public static final int NULL_PATH = 970;

    /**
     * Status constant indicating that a path outside of the project was
     * supplied to the operation. The path can be retrieved using
     * <code>getPath</code> on the status object.
     */
    public static final int PATH_OUTSIDE_PROJECT = 971;

    /**
     * Status constant indicating that a relative path was supplied to the
     * operation when an absolute path is required. The path can be retrieved
     * using <code>getPath</code> on the status object.
     */
    public static final int RELATIVE_PATH = 972;

    /**
     * Status constant indicating that a path specifying a device was supplied
     * to the operation when a path with no device is required. The path can be
     * retrieved using <code>getPath</code> on the status object.
     */
    public static final int DEVICE_PATH = 973;

    /**
     * Status constant indicating that a string was supplied to the operation
     * that was <code>null</code>.
     */
    public static final int NULL_STRING = 974;

    /**
     * Status constant indicating that the operation encountered a read-only
     * element. The element(s) can be retrieved using <code>getElements</code>
     * on the status object.
     */
    public static final int READ_ONLY = 976;

    /**
     * Status constant indicating that a naming collision would occur if the
     * operation proceeded.
     */
    public static final int NAME_COLLISION = 977;

    /**
     * Status constant indicating that a destination provided for a
     * copy/move/rename operation is invalid. The destination element can be
     * retrieved using <code>getElements</code> on the status object.
     */
    public static final int INVALID_DESTINATION = 978;

    /**
     * Status constant indicating that a path provided to an operation is
     * invalid. The path can be retrieved using <code>getPath</code> on the
     * status object.
     */
    public static final int INVALID_PATH = 979;

    /**
     * Status constant indicating the given source position is out of bounds.
     */
    public static final int INDEX_OUT_OF_BOUNDS = 980;

    /**
     * Status constant indicating there is an update conflict for a working
     * copy. The compilation unit on which the working copy is based has changed
     * since the working copy was created.
     */
    public static final int UPDATE_CONFLICT = 981;

    /**
     * Status constant indicating that <code>null</code> was specified as a name
     * argument.
     */
    public static final int NULL_NAME = 982;

    /**
     * Status constant indicating that a name provided is not syntactically
     * correct. The name can be retrieved from <code>getString</code>.
     */
    public static final int INVALID_NAME = 983;

    /**
     * Status constant indicating that the specified contents are not valid.
     */
    public static final int INVALID_CONTENTS = 984;

    /**
     * Status constant indicating that an <code>java.io.IOException</code>
     * occurred.
     */
    public static final int IO_EXCEPTION = 985;

    /**
     * Status constant indicating that the Erlang builder could not be
     * initialized.
     */
    public static final int BUILDER_INITIALIZATION_ERROR = 990;

    /**
     * Status constant indicating that the Erlang builder's last built state
     * could not be serialized or deserialized.
     */
    public static final int BUILDER_SERIALIZATION_ERROR = 991;

    /**
     * Status constant indicating that a sibling specified is not valid.
     */
    public static final int INVALID_SIBLING = 993;

    /**
     * Status indicating that a Erlang element could not be created because the
     * underlying resource is invalid.
     * 
     * @see ErlangCore
     */
    public static final int INVALID_RESOURCE = 995;

    /**
     * Status indicating that a Erlang element could not be created because the
     * underlying resource is not of an appropriate type.
     * 
     * @see ErlangCore
     */
    public static final int INVALID_RESOURCE_TYPE = 996;

    /**
     * Status indicating that a Erlang element could not be created because the
     * project owning underlying resource does not have the Erlang nature.
     * 
     * @see ErlangCore
     */
    public static final int INVALID_PROJECT = 997;

    /**
     * Status indicating that the corresponding resource has no local contents
     * yet. This might happen when attempting to use a resource before its
     * contents has been made locally available.
     */
    public static final int NO_LOCAL_CONTENTS = 999;

    /**
     * Status indicating that a .classpath file is ill-formed, and thus cannot
     * be read/written successfully.
     */
    public static final int INVALID_CODEPATH_FILE_FORMAT = 1000;

    /**
     * Status constant indicating that a project is prerequisiting some library
     * for which the classfile ERTS version level is more recent than the
     * project ERTS target level setting. This can indicate some binary
     * incompatibility issues later on.
     */
    public static final int INCOMPATIBLE_ERTS_LEVEL = 1004;

    /**
     * Status constant indicating that a compiler failure occurred.
     */
    public static final int COMPILER_FAILURE = 1005;

    /**
	 *
	 */
    public static final int INVALID_PATHENTRY = 1006;

    private ErlModelStatusConstants() {
    }
}
