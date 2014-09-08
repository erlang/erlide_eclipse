package org.erlide.core.internal.dev;

import java.io.PrintStream;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.SourcePathProvider;
import org.erlide.engine.model.SourcePathUtils;
import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImportExport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ParserDB {

    private static ParserDB db;
    private static PrintStream out;
    private static volatile boolean running = false;

    public static void create() {
        final Runnable x = new Runnable() {
            @Override
            public void run() {
                running = true;
                try {
                    db = new ParserDB();
                    // out = new PrintStream(
                    // new File("/home/qvladum/parserDB.txt"));
                    out = System.out;

                    final IErlModel model = ErlangEngine.getInstance().getModel();
                    final Collection<SourcePathProvider> sourcePathProviders = SourcePathUtils
                            .getSourcePathProviders();
                    final long time = System.currentTimeMillis();
                    db.run(model, sourcePathProviders, false);
                    System.out.println(" took " + (System.currentTimeMillis() - time)
                            / 1000);
                } catch (final Exception e) {
                    ErlLogger.error(e);
                }
                running = false;
            }
        };
        if (!running) {
            new Thread(x).start();
        }
    }

    private void run(final IErlModel model,
            final Collection<SourcePathProvider> sourcePathProviders,
            final boolean includeTests) throws ErlModelException {
        // we include all projects in workspace - create one for OTP too

        int normal = 0;
        int test = 0;

        final Collection<IErlProject> projects = model.getErlangProjects();
        for (final IErlProject project : projects) {
            final Collection<IErlModule> modules = project.getModulesAndIncludes();
            for (final IErlModule module : modules) {
                if (isTest(module.getResource().getLocation().toPortableString())) {
                    test++;
                } else {
                    normal++;
                }
                handleModule(module);
            }
            if (includeTests) {
                // final TestCodeBuilder builder = new TestCodeBuilder();
                // try {
                // final Set<BuildResource> resources = builder
                // .getAffectedResources(
                // (IProject) project.getResource(),
                // new NullProgressMonitor(), false);
                // for (final BuildResource res : resources) {
                // if (res.getResource().isLinked()
                // || res.getResource().isDerived()) {
                // System.out.println("SKIP!!!! " + res.getResource());
                // continue;
                // }
                // test++;
                // final IErlModule module = ErlModelManager
                // .getInstance().getModel().findModule(
                // (IFile) res.getResource());
                // handleModule(module);
                // }
                // } catch (final CoreException e) {
                // ErlLogger.error(e);
                // }
            }
        }
        out.println("--- " + normal + " " + test);
        System.out.println("--- " + normal + " " + test);
    }

    public void handleModule(final IErlModule module) throws ErlModelException {
        module.open(null);
        final int numForms = module.getChildCount();
        final String path = module.getResource().getLocation().toPortableString();
        out.println(path + " " + numForms + " " + isTest(path));
        System.out.println(path + " " + numForms + " " + isTest(path));
        for (final IErlElement form : module.getChildren()) {
            out.print(" " + form.getKind() + " ");
            if (form instanceof IErlImportExport) {
                final IErlImportExport export = (IErlImportExport) form;
                out.println(export.getFunctions().size());
            } else if (form instanceof IErlPreprocessorDef) {
                final IErlPreprocessorDef def = (IErlPreprocessorDef) form;
                out.println(fix(def.getDefinedName()));
            } else if (form instanceof IErlTypespec) {
                final IErlTypespec attribute = (IErlTypespec) form;
                out.println("TYPESPEC " + fix(attribute.getName()));
            } else if (form instanceof IErlAttribute) {
                final IErlAttribute attribute = (IErlAttribute) form;
                out.println(fix(attribute.getName()));
            } else if (form instanceof IErlFunction) {
                final IErlFunction function = (IErlFunction) form;
                int numClauses = function.getChildCount();
                numClauses = numClauses == 0 ? 1 : numClauses;
                out.println(fix(function.getName()) + " " + function.getArity() + " "
                        + numClauses);
            } else {
                out.println("?? " + form.getClass().getName());
            }
        }
        module.close();
        module.dispose();
    }

    private String fix(final String name) {
        return name.trim().replace(' ', '^');
    }

    public List<String> getTestSourcePaths(
            final Collection<SourcePathProvider> sourcePathProviders,
            final Collection<IErlProject> projects) {
        final List<String> spaths = Lists.newArrayList();
        for (final IErlProject project : projects) {
            final Set<IPath> paths = Sets.newHashSet();
            for (final SourcePathProvider provider : sourcePathProviders) {
                final IProject resource = (IProject) project.getResource();
                paths.addAll(provider.getSourcePathsForModel(resource));
            }
            for (final IPath path : paths) {
                spaths.add(path.toPortableString());
            }
        }
        Collections.sort(spaths);
        return spaths;
    }

    private boolean isTest(final String path) {
        return path.contains("/test/") || path.contains("_SUITE");
    }
}
