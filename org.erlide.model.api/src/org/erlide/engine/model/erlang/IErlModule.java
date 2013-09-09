/*******************************************************************************
// * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     IBM Corporation - added J2SE 1.5 support
 *     Vlad Dumitrescu
 * *******************************************************************************/
package org.erlide.engine.model.erlang;

import java.util.Collection;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.ISourceUnit;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.util.ErlangFunction;
import org.erlide.engine.util.ErlangIncludeFile;

/**
 * Represents an entire Erlang compilation unit (<code>.erl</code> or
 * <code>.hrl</code> source file). Compilation unit elements need to be opened
 * before they can be navigated or manipulated. The children are of type
 * <code>IErlAttribute</code>, and <code>IErlFunction</code>, and appear in the
 * order in which they are declared in the source. If a <code>.erl</code> file
 * cannot be parsed, its structure remains unknown. Use
 * <code>IErlElement.isStructureKnown</code> to determine whether this is the
 * case.
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 */
public interface IErlModule extends IErlElement, IParent, IOpenable,
        ISourceUnit {

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

    /**
     * Is this module a real one, or an include file?
     * 
     * @return true if .erl, false if .hrl
     */
    ModuleKind getModuleKind();

    Collection<IErlComment> getComments();

    long getTimestamp();

    IErlImport findImport(ErlangFunction function);

    Collection<IErlImport> getImports();

    IErlPreprocessorDef findPreprocessorDef(String definedName,
            ErlElementKind kind);

    public Collection<IErlPreprocessorDef> getPreprocessorDefs(
            final ErlElementKind kind);

    Collection<ErlangIncludeFile> getIncludeFiles() throws ErlModelException;

    // void getScanner();
    //
    // void disposeScanner();

    void initialReconcile();

    void reconcileText(int offset, int removeLength, String newText,
            IProgressMonitor mon);

    void postReconcile(IProgressMonitor mon);

    void finalReconcile();

    /**
     * Returns a collection of modules that include this one.
     **/
    Set<IErlModule> getDirectDependentModules() throws ErlModelException;

    /**
     * Returns the transitive closure of modules that include this one.
     * 
     * @throws CoreException
     **/
    Set<IErlModule> getAllDependentModules() throws CoreException;

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

    boolean getLogging();

}
