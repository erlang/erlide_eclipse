/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang;

import org.eclipse.core.runtime.IPath;

public interface IErlCodepathEntry {

	/**
	 * Entry kind constant describing a classpath entry identifying a library. A
	 * library is a folder or JAR containing package fragments consisting of
	 * pre-compiled binaries.
	 */
	int CPE_LIBRARY = 1;

	/**
	 * Entry kind constant describing a classpath entry identifying a required
	 * project.
	 */
	int CPE_PROJECT = 2;

	/**
	 * Entry kind constant describing a classpath entry identifying a folder
	 * containing package fragments with source code to be compiled.
	 */
	int CPE_SOURCE = 3;

	/**
	 * Entry kind constant describing a classpath entry defined using a path
	 * that begins with a classpath variable reference.
	 */
	int CPE_VARIABLE = 4;

	/**
	 * Entry kind constant describing a classpath entry representing a name
	 * classpath container.
	 * 
	 * @since 2.0
	 */
	int CPE_CONTAINER = 5;

	/**
	 * Returns the kind of files found in the package fragments identified by
	 * this classpath entry.
	 * 
	 * @return <code>IPackageFragmentRoot.K_SOURCE</code> for files containing
	 *         source code, and <code>IPackageFragmentRoot.K_BINARY</code> for
	 *         binary class files. There is no specified value for an entry
	 *         denoting a variable (<code>CPE_VARIABLE</code>) or a
	 *         classpath container (<code>CPE_CONTAINER</code>).
	 */
	int getContentKind();

	/**
	 * Returns the kind of this classpath entry.
	 * 
	 * @return one of:
	 *         <ul>
	 *         <li><code>CPE_SOURCE</code> - this entry describes a source
	 *         root in its project
	 *         <li><code>CPE_LIBRARY</code> - this entry describes a folder
	 *         or JAR containing binaries
	 *         <li><code>CPE_PROJECT</code> - this entry describes another
	 *         project
	 * 
	 * <li><code>CPE_VARIABLE</code> - this entry describes a project or
	 * library indirectly via a classpath variable in the first segment of the
	 * path *
	 * <li><code>CPE_CONTAINER</code> - this entry describes set of entries
	 * referenced indirectly via a classpath container
	 * </ul>
	 */
	int getEntryKind();

	/**
	 * Returns the full path to the specific location where the builder writes
	 * <code>.class</code> files generated for this source entry (entry kind
	 * <code>CPE_SOURCE</code>).
	 * <p>
	 * Source entries can optionally be associated with a specific output
	 * location. If none is provided, the source entry will be implicitly
	 * associated with its project default output location (see
	 * <code>IJavaProject#getOutputLocation</code>).
	 * </p>
	 * <p>
	 * NOTE: A specific output location cannot coincidate with another
	 * source/library entry.
	 * </p>
	 * 
	 * @return the full path to the specific location where the builder writes
	 *         <code>.class</code> files for this source entry, or
	 *         <code>null</code> if using default output folder
	 * @since 2.1
	 */
	IPath getOutputLocation();

	/**
	 * Returns the path of this classpath entry.
	 * 
	 * The meaning of the path of a classpath entry depends on its entry kind:
	 * <ul>
	 * <li>Source code in the current project (<code>CPE_SOURCE</code>) -
	 * The path associated with this entry is the absolute path to the root
	 * folder. </li>
	 * <li>A binary library in the current project (<code>CPE_LIBRARY</code>) -
	 * the path associated with this entry is the absolute path to the JAR (or
	 * root folder), and in case it refers to an external JAR, then there is no
	 * associated resource in the workbench.
	 * <li>A required project (<code>CPE_PROJECT</code>) - the path of the
	 * entry denotes the path to the corresponding project resource.</li>
	 * <li>A variable entry (<code>CPE_VARIABLE</code>) - the first segment
	 * of the path is the name of a classpath variable. If this classpath
	 * variable is bound to the path <it>P</it>, the path of the corresponding
	 * classpath entry is computed by appending to <it>P</it> the segments of
	 * the returned path without the variable.</li>
	 * <li> A container entry (<code>CPE_CONTAINER</code>) - the path of the
	 * entry is the name of the classpath container, which can be bound
	 * indirectly to a set of classpath entries after resolution. The
	 * containerPath is a formed by a first ID segment followed with extra
	 * segments that can be used as additional hints for resolving this
	 * container reference (also see <code>IClasspathContainer</code>). </li>
	 * </ul>
	 * 
	 * @return the path of this classpath entry
	 */
	IPath getPath();

	/**
	 * Returns the path to the source archive or folder associated with this
	 * classpath entry, or <code>null</code> if this classpath entry has no
	 * source attachment.
	 * <p>
	 * Only library and variable classpath entries may have source attachments.
	 * For library classpath entries, the result path (if present) locates a
	 * source archive or folder. This archive or folder can be located in a
	 * project of the workspace or outside thr workspace. For variable classpath
	 * entries, the result path (if present) has an analogous form and meaning
	 * as the variable path, namely the first segment is the name of a classpath
	 * variable.
	 * </p>
	 * 
	 * @return the path to the source archive or folder, or <code>null</code>
	 *         if none
	 */
	IPath getSourceAttachmentPath();

	/**
	 * Returns the path within the source archive or folder where package
	 * fragments are located. An empty path indicates that packages are located
	 * at the root of the source archive or folder. Returns a non-<code>null</code>
	 * value if and only if <code>getSourceAttachmentPath</code> returns a
	 * non-<code>null</code> value.
	 * 
	 * @return the path within the source archive or folder, or
	 *         <code>null</code> if not applicable
	 */
	IPath getSourceAttachmentRootPath();

}
