package org.erlide.shade.bterl.ui;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.shade.bterl.builder.ErlTestNature;
import org.erlide.shade.bterl.ui.launcher.TestLaunchShortcut;

public class TestRunnableTester extends PropertyTester {

    public TestRunnableTester() {
    }

    enum RunKind {
        NONE, RUN, COVER, TRACE, DEBUG, REGRESSION;

        public static RunKind build(final String prop) {
            if ("canRun".equals(prop)) {
                return RUN;
            } else if ("canCover".equals(prop)) {
                return COVER;
            } else if ("canTrace".equals(prop)) {
                return TRACE;
            } else if ("canDebug".equals(prop)) {
                return DEBUG;
            } else if ("canRegression".equals(prop)) {
                return REGRESSION;
            } else {
                return NONE;
            }
        }
    }

    /**
     * Can run if it is a file with name matching "*_SUITE.erl" or a folder
     * containing such a file directly beneath it.
     */
    @Override
    public boolean test(Object receiver, final String property,
            final Object[] args, final Object expectedValue) {
        if (receiver instanceof IEditorPart
                || receiver instanceof IFileEditorInput) {
            // maybe we should use adapters?
            receiver = TestLaunchShortcut.getEditorTarget(receiver);
        }

        final RunKind kind = RunKind.build(property);
        final boolean isTestDir = isTestDir(receiver);
        final boolean isTestSuite = isTestSuite(receiver);
        final boolean isTestCase = isTestCase(receiver);
        switch (kind) {
        case RUN:
            return isTestDir || isTestSuite || isTestCase;
        case COVER:
            return isTestDir;
        case TRACE:
            return isTestCase;
        case DEBUG:
            return isTestSuite || isTestCase;
        case REGRESSION:
            return isBterlProject(receiver);
        default:
            return false;
        }
    }

    static class BterlVisitor implements IResourceVisitor {
        boolean hasTests = false;

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            if (isTestSuite(resource)) {
                hasTests = true;
            }
            return true;
        }

    }

    static boolean isTestDir(final Object receiver) {
        if (receiver instanceof IFolder) {
            final IFolder res = (IFolder) receiver;
            if (!isBterlProject(res.getProject())) {
                return false;
            }
            try {
                final BterlVisitor visitor = new BterlVisitor();
                res.accept(visitor, 1, false);
                return visitor.hasTests;
            } catch (final CoreException e) {
            }
            return false;
        }
        return false;
    }

    static boolean isTestSuite(final Object receiver) {
        if (receiver instanceof IErlModule) {
            final IErlModule mod = (IErlModule) receiver;
            final IResource file = mod.getResource();
            return isTestSuite(file);
        }
        IFile res = null;
        if (receiver instanceof IFile) {
            res = (IFile) receiver;
        } else if (receiver instanceof IFileEditorInput) {
            res = ((IFileEditorInput) receiver).getFile();
        }
        if (res == null || !isBterlProject(res.getProject())) {
            return false;
        }
        return res.getName().matches(".*_SUITE.erl");
    }

    static boolean isTestCase(final Object receiver) {
        if (receiver instanceof IErlFunction) {
            final IErlFunction fun = (IErlFunction) receiver;
            final IErlModule mod = fun.getModule();
            final IResource file = mod.getResource();
            return isTestSuite(file) && fun.isExported();
        }
        if (receiver instanceof IErlFunctionClause) {
            final IErlFunctionClause clause = (IErlFunctionClause) receiver;
            final ErlangFunction fc = new ErlangFunction(
                    clause.getFunctionName(), clause.getArity());
            final IErlFunction fun = clause.getModule().findFunction(fc);
            return fun != null && isTestCase(fun);
        }
        return false;
    }

    static boolean isBterlProject(final Object receiver) {
        if (receiver instanceof IProject) {
            final IProject project = (IProject) receiver;
            try {
                return project.hasNature(ErlTestNature.NATURE_ID);
            } catch (final CoreException e) {
                // ignore
            }
        }
        return false;
    }
}
