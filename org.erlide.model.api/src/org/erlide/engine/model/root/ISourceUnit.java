/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.ErlangIncludeFile;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.services.parsing.ScannerService;

/**
 *
 * @author Vlad
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface ISourceUnit {

    /**
     * Is this unit a module or an include file?
     */
    SourceKind getSourceKind();

    /**
     * Returns the smallest element within this module that includes the given
     * source position (that is, a clause, attribute, etc.), or
     * <code>null</code> if there is no element other than the compilation unit
     * itself at the given position, or if the given position is not within the
     * source range of this compilation unit.
     *
     * @param position
     *            a source position inside the compilation unit
     * @return the innermost Erlang element enclosing a given source position or
     *         <code>null</code> if none (excluding the compilation unit).
     * @throws ErlModelException
     *             if the compilation unit does not exist or if an exception
     *             occurs while accessing its corresponding resource
     */
    IErlElement getElementAt(int position) throws ErlModelException;

    IErlMember getElementAtLine(int lineNumber);

    Collection<IErlComment> getComments();

    IErlImport findImport(ErlangFunction function);

    Collection<IErlImport> getImports();

    IErlPreprocessorDef findPreprocessorDef(String definedName, ErlElementKind kind);

    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final ErlElementKind kind);

    Collection<ErlangIncludeFile> getIncludeFiles() throws ErlModelException;

    void initialReconcile();

    void reconcileText(int offset, int removeLength, String newText, IProgressMonitor mon);

    void postReconcile(IProgressMonitor mon);

    void finalReconcile();

    /**
     * Returns a collection of modules that include this one.
     **/
    Set<ISourceUnit> getDirectDependentModules() throws ErlModelException;

    /**
     * Returns the transitive closure of modules that include this one.
     *
     * @throws CoreException
     **/
    Set<ISourceUnit> getAllDependentModules() throws CoreException;

    /**
     * Resets parser so that the next parse will be a full parse, possibly
     * updating the parser cache
     *
     * @param newText
     * @throws ErlModelException
     */
    void resetAndCacheScannerAndParser(String newText) throws ErlModelException;

    /**
     * Get the module name without extension
     *
     * @return name as string
     */
    String getModuleName();

    IErlFunction findFunction(ErlangFunction erlangFunction);

    IErlTypespec findTypespec(String typeName);

    void setResource(IFile file);

    void setComments(Collection<? extends IErlComment> comments);

    boolean isOnSourcePath();

    boolean isOnIncludePath();

    boolean exportsAllFunctions();

    String getScannerName();

    ScannerService getScanner();

    void createScanner();

}
