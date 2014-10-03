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
package org.erlide.engine.internal.model.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ModelInternalUtils implements ModelUtilService {

    private final IOtpRpc backend;

    public ModelInternalUtils(final IOtpRpc backend) {
        this.backend = backend;
    }

    private static final String DELIMITER = "<>";

    @Override
    public String getExternalModulePath(final IErlElementLocator model,
            final IErlModule module) {
        final List<String> result = Lists.newArrayList();
        IErlElement element = module;
        while (element != model) {
            if (element instanceof IErlExternal) {
                final IErlExternal external = (IErlExternal) element;
                result.add(external.getName());
            } else {
                result.add(element.getName());
            }
            element = (IErlElement) element.getParent();
        }
        return Joiner.on(DELIMITER).join(Lists.reverse(result));
    }

    private IErlExternal getElementWithExternalName(final IParent parent,
            final String segment) throws ErlModelException {
        for (final IErlElement i : parent.getChildrenOfKind(ErlElementKind.EXTERNAL_ROOT,
                ErlElementKind.EXTERNAL_APP, ErlElementKind.EXTERNAL_FOLDER)) {
            final IErlExternal external = (IErlExternal) i;
            final String externalName = external.getName();
            if (externalName.equals(segment)) {
                return external;
            }
        }
        return null;
    }

    @Override
    public IErlModule getModuleFromExternalModulePath(final IErlModel model,
            final String modulePath) throws ErlModelException {
        final List<String> path = Lists.newArrayList(Splitter.on(DELIMITER).split(
                modulePath));
        model.open(null);
        final IErlElement childNamed = model.getChildNamed(path.get(0));
        if (childNamed instanceof IParent) {
            IParent parent = (IParent) childNamed;
            final int n = path.size() - 1;
            for (int i = 1;; i++) {
                if (parent == null) {
                    break;
                }
                if (parent instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) parent;
                    openable.open(null);
                }
                if (i == n) {
                    break;
                }
                parent = getElementWithExternalName(parent, path.get(i));
            }
            if (parent != null) {
                final IErlElement child = parent.getChildNamed(path.get(n));
                if (child instanceof IErlModule) {
                    return (IErlModule) child;
                }
            }
        }
        return null;
    }

    @Override
    public List<String> findUnitsWithPrefix(final String prefix,
            final IErlProject project, final boolean checkExternals,
            final boolean includes) throws ErlModelException {
        final List<String> result = Lists.newArrayList();
        final Collection<IErlModule> units = getUnits(project, checkExternals, includes);
        addUnitNamesWithPrefix(prefix, result, units, false, includes);
        if (project != null) {
            for (final IErlProject p : project.getReferencedProjects()) {
                if (p != null) {
                    p.open(null);
                    addUnitNamesWithPrefix(prefix, result,
                            getUnits(p, checkExternals, includes), false, includes);
                }
            }
            if (checkExternals) {
                final Collection<IErlModule> externalUnits = includes ? project
                        .getExternalIncludes() : project.getExternalModules();
                addUnitNamesWithPrefix(prefix, result, externalUnits, true, includes);
            }
        }
        return result;
    }

    private Collection<IErlModule> getUnits(final IErlProject project,
            final boolean checkExternals, final boolean includes)
            throws ErlModelException {
        final Collection<IErlModule> units;
        if (!includes && project != null) {
            units = project.getModules();
        } else if (!checkExternals && project != null) {
            units = project.getIncludes();
        } else {
            units = Sets.newHashSet();
        }
        return units;
    }

    @Override
    public List<OtpErlangObject> getImportsAsList(final IErlModule mod) {
        if (mod == null) {
            return NO_IMPORTS;
        }
        final Collection<IErlImport> imports = mod.getImports();
        if (imports.isEmpty()) {
            return NO_IMPORTS;
        }
        final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                imports.size());
        for (final IErlImport i : imports) {
            final Collection<ErlangFunction> functions = i.getFunctions();
            final OtpErlangObject funsT[] = new OtpErlangObject[functions.size()];
            int j = 0;
            for (final ErlangFunction f : functions) {
                funsT[j] = f.getNameArityTuple();
                j++;
            }
            final OtpErlangTuple modFunsT = new OtpErlangTuple(new OtpErlangObject[] {
                    new OtpErlangAtom(i.getImportModule()), new OtpErlangList(funsT) });
            result.add(modFunsT);
        }
        return result;
    }

    @Override
    public List<IErlPreprocessorDef> getAllPreprocessorDefs(final IErlModule module,
            final ErlElementKind kind) throws CoreException {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        final List<IErlModule> modulesWithIncludes = Lists.newArrayList(ErlangEngine
                .getInstance().getModelSearcherService().findAllIncludedFiles(module));
        modulesWithIncludes.add(module);
        for (final IErlModule m : modulesWithIncludes) {
            result.addAll(m.getPreprocessorDefs(kind));
        }
        return result;
    }

    public static final List<OtpErlangObject> NO_IMPORTS = new ArrayList<OtpErlangObject>(
            0);

    private void addUnitNamesWithPrefix(final String prefix, final List<String> result,
            final Collection<IErlModule> modules, final boolean external,
            final boolean includes) {
        for (final IErlModule module : modules) {
            String moduleName = includes ? module.getName() : module.getModuleName();
            if (external && includes) {
                moduleName = getIncludeLibPath(module);
            }
            boolean nameMatches = moduleName.startsWith(prefix);
            if (!nameMatches && prefix.startsWith("'")) {
                nameMatches = moduleName.startsWith(prefix.substring(1));
            }
            if (nameMatches && (includes || !module.getName().endsWith(".hrl"))) {
                if (!result.contains(moduleName)) {
                    final String name = new OtpErlangAtom(moduleName).toString();
                    result.add(name);
                }
            }
        }
    }

    private String getIncludeLibPath(final IErlModule module) {
        String s = module.getName();
        String prevS = s;
        IErlElement e = module;
        for (;;) {
            final IParent p = e.getParent();
            if (p instanceof IErlProject) {
                break;
            }
            e = (IErlElement) p;
            prevS = s;
            s = e.getName() + "/" + s;
        }
        return prevS;
    }

    @Override
    public String[] getPredefinedMacroNames() {
        return new String[] { "MODULE", "LINE", "FILE" };
    }

    @Override
    public boolean isOtpModule(final IErlModule module) {
        IParent parent = module.getParent();
        while (parent instanceof IErlExternal) {
            final IErlExternal external = (IErlExternal) parent;
            if (external.isOTP()) {
                return true;
            }
            parent = external.getParent();
        }
        return false;
    }

    @Override
    public IErlModule getModule(final IErlElement element) {
        if (element instanceof IErlModule) {
            return (IErlModule) element;
        }
        return (IErlModule) element.getAncestorOfKind(ErlElementKind.MODULE);
    }

    @Override
    public IErlProject getProject(final IErlElement element) {
        if (element == null) {
            return null;
        }
        final IErlElement ancestor = element.getAncestorOfKind(ErlElementKind.PROJECT);
        if (ancestor instanceof IErlProject) {
            return (IErlProject) ancestor;
        }
        return null;
    }

    /**
     * Helper method - returns the targeted item (IResource if internal or
     * java.io.File if external), or null if unbound Internal items must be
     * referred to using container relative paths.
     */
    @Override
    public Object getTarget(final IContainer container, final IPath path,
            final boolean checkResourceExistence) {

        if (path == null) {
            return null;
        }

        // lookup - inside the container
        if (path.getDevice() == null) { // container relative paths should not
            // contain a device
            // (see http://dev.eclipse.org/bugs/show_bug.cgi?id=18684)
            // (case of a workspace rooted at d:\ )
            final IResource resource = container.findMember(path);
            if (resource != null) {
                if (!checkResourceExistence || resource.exists()) {
                    return resource;
                }
                return null;
            }
        }

        // if path is relative, it cannot be an external path
        // (see http://dev.eclipse.org/bugs/show_bug.cgi?id=22517)
        if (!path.isAbsolute()) {
            return null;
        }

        // lookup - outside the container
        final File externalFile = new File(path.toOSString());
        if (!checkResourceExistence) {
            return externalFile;
        }
        if (externalFile.exists()) {
            return externalFile;
        }
        return null;
    }

    @Override
    public String getModuleInfo(final IErlModule module) {
        final String errorValue = "There is no module information about this file.";
        if (module == null) {
            return errorValue;
        }

        final IErlProject project = getProject(module);
        final IPath beamPath = project.getProperties().getOutputDir()
                .append(module.getModuleName()).addFileExtension("beam");
        final IFile beam = project.getWorkspaceProject().getFile(beamPath);

        try {
            final OtpErlangObject info = backend.call("erlide_backend",
                    "get_module_info", "s", beam.getLocation().toPortableString());
            return (String) TypeConverter.erlang2java(info, String.class);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return errorValue;
    }
}
