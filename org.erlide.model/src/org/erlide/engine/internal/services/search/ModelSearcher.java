package org.erlide.engine.internal.services.search;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangIncludeFile;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelSearcherService;
import org.erlide.engine.services.search.ModelUtilService;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ModelSearcher implements ModelSearcherService {

    private final ModelUtilService modelUtilService;

    public ModelSearcher() {
        modelUtilService = ErlangEngine.getInstance().getModelUtilService();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.erlide.engine.model.ModelSearcherService#findAllIncludedFiles(org
     * .erlide.engine.model.erlang.IErlModule)
     */
    @Override
    public Collection<IErlModule> findAllIncludedFiles(final IErlModule module)
            throws CoreException {
        final List<IErlModule> checked = Lists.newArrayList();
        return findAllIncludedFiles(module, checked);
    }

    public Collection<IErlModule> findAllIncludedFiles(final IErlModule module,
            final List<IErlModule> checked) throws CoreException {
        final Collection<IErlModule> result = Sets.newHashSet();

        if (checked.contains(module)) {
            return result;
        }
        checked.add(module);

        final List<IErlModule> includedFilesForModule = ErlModelCache.getDefault()
                .getIncludedFilesForModule(module);
        if (includedFilesForModule != null && !includedFilesForModule.isEmpty()) {
            return includedFilesForModule;
        }
        final Collection<ErlangIncludeFile> includedFiles = module.getIncludeFiles();
        final IErlProject project = modelUtilService.getProject(module);
        if (project == null) {
            return result;
        }
        final Collection<IErlModule> includes = project.getIncludes();
        includes.addAll(getLocalIncludes(module));
        Collection<IErlModule> externalIncludes = null;
        Collection<IErlModule> referencedIncludes = null;
        Collection<IErlModule> modules = null;
        for (final ErlangIncludeFile includeFile : includedFiles) {
            final String includeFileName = includeFile.getFilenameLastPart();
            if (findAllIncludedFilesAux(checked, result, includes, includeFileName)) {
                continue;
            }
            if (referencedIncludes == null) {
                referencedIncludes = Lists.newArrayList();
                final Collection<IErlProject> referencedProjects = project
                        .getReferencedProjects();
                for (final IErlProject referencedProject : referencedProjects) {
                    referencedIncludes.addAll(referencedProject.getIncludes());
                }
            }
            if (findAllIncludedFilesAux(checked, result, referencedIncludes,
                    includeFileName)) {
                continue;
            }
            if (externalIncludes == null) {
                externalIncludes = project.getExternalIncludes();
            }
            if (findAllIncludedFilesAux(checked, result, externalIncludes,
                    includeFileName)) {
                continue;
            }
            if (modules == null) {
                modules = project.getModules();
            }
            findAllIncludedFilesAux(checked, result, modules, includeFileName);
        }
        ErlModelCache.getDefault().putIncludedFilesForModule(module, result);
        return result;
    }

    private Collection<IErlModule> getLocalIncludes(final IErlModule module)
            throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        final IParent parent = module.getParent();
        for (final IErlElement child : parent.getChildrenOfKind(ErlElementKind.MODULE)) {
            if (child instanceof IErlModule
                    && SourceKind.nameToModuleKind(child.getName()) == SourceKind.HRL) {
                result.add((IErlModule) child);
            }
        }
        return result;
    }

    private boolean findAllIncludedFilesAux(final List<IErlModule> checked,
            final Collection<IErlModule> result, final Collection<IErlModule> includes,
            final String includeFileName) throws CoreException {
        for (final IErlModule include : includes) {
            if (include.getName().equals(includeFileName)) {
                if (include.getParent() instanceof IErlExternal) {
                    result.add(findExternalIncludeInOpenProjects(include));
                } else {
                    result.add(include);
                }
                result.addAll(findAllIncludedFiles(include, checked));
                return true;
            }
        }
        return false;
    }

    public static IErlModule findExternalIncludeInOpenProjects(
            final IErlModule externalInclude) throws CoreException {
        final String filePath = externalInclude.getFilePath();
        final Collection<IErlProject> projects = ErlangEngine.getInstance().getModel()
                .getErlangProjects();
        for (final IErlProject project : projects) {
            final Collection<IErlModule> includes = project.getIncludes();
            for (final IErlModule include : includes) {
                if (include.getFilePath().equals(filePath)) {
                    return include;
                }
            }
        }
        return externalInclude;
    }

}
