package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.test_support.ui.suites.TestResultsView;

public class TestLaunchShortcut implements ILaunchShortcut {

    @Override
    public void launch(final ISelection selection, final String mode) {
        if (selection instanceof IStructuredSelection) {
            final Object item = ((IStructuredSelection) selection)
                    .getFirstElement();
            doLaunch(item, mode);
        }
    }

    @Override
    public void launch(final IEditorPart editor, final String mode) {
        doLaunch(editor, mode);
    }

    public void doLaunch(final Object target, final String mode) {
        final ILaunchConfiguration launchConfig = getLaunchConfiguration(target);
        try {
            if (launchConfig != null) {
                final TestResultsView view = (TestResultsView) PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().showView(TestResultsView.VIEW_ID);
                if (view != null) {
                    view.clearEvents();
                    view.setMessage("Launching: " + getTargetName(target));

                    final ILaunch launch = launchConfig
                            .launch(mode, Job.getJobManager()
                                    .createProgressGroup(), false, true);
                    final IBackend backend = BackendCore.getBackendManager()
                            .getBackendForLaunch(launch);
                    if (backend == null) {
                        System.out.println("NULL backend for bterl");
                        return;
                    }
                    view.getEventHandler().register();
                }
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        }
    }

    protected ILaunchConfiguration getLaunchConfiguration(final Object target) {
        final String targetName = getTargetName(target);
        if (targetName == null) {
            return null;
        }
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType("org.erlide.test_support.launchConfigurationType");
        ILaunchConfigurationWorkingCopy workingCopy;
        try {
            workingCopy = type.newInstance(null,
                    "internal_" + targetName.replace('/', '_'));
            return workingCopy;
        } catch (final CoreException e) {
            e.printStackTrace();
            return null;
        }
    }

    protected String getTargetName(final Object target) {
        Object newtarget = target;
        if (target instanceof IEditorPart) {
            newtarget = getEditorTarget(target);
        }
        if (newtarget == null) {
            return null;
        }
        if (newtarget instanceof IProject) {
            return ((IProject) newtarget).getName();
        }
        if (newtarget instanceof IResource) {
            return ((IResource) newtarget).getProjectRelativePath().toString();
        }
        String targetName = newtarget.toString();
        if (newtarget instanceof IErlFunctionClause) {
            targetName = ((IErlFunctionClause) newtarget).getFunctionName();
        }
        return targetName;
    }

    public static Object getEditorTarget(final Object target) {
        Object result = null;
        IEditorInput input = null;
        if (target instanceof IEditorPart) {
            final IEditorPart editor = (IEditorPart) target;
            input = editor.getEditorInput();
        } else if (target instanceof IEditorInput) {
            input = (IEditorInput) target;
        }
        if (input instanceof IFileEditorInput) {
            final IFile file = ((IFileEditorInput) input).getFile();
            final IWorkbench workbench = PlatformUI.getWorkbench();
            final IWorkbenchWindow win = workbench.getActiveWorkbenchWindow();
            if (win != null) {
                final IWorkbenchPage page = win.getActivePage();
                final IEditorPart part = page.findEditor(input);
                final ISelection sel = part.getEditorSite()
                        .getSelectionProvider().getSelection();
                if (sel instanceof ITextSelection && !sel.isEmpty()) {
                    final ITextSelection tsel = (ITextSelection) sel;
                    try {
                        final IErlModule module = ErlModelManager
                                .getErlangModel().findModule(file);
                        if (module != null) {
                            result = module.getElementAt(tsel.getOffset());
                            result = getFunction(result);
                        }
                    } catch (final ErlModelException e) {
                    }
                    if (result == null) {
                        result = file;
                    }
                } else {
                    result = file;
                }
            } else {
                result = file;
            }
        }
        return result;
    }

    @SuppressWarnings("deprecation")
    protected String getResolvedPath(final IResource dir) {
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        IPath loc = dir.getRawLocation();
        loc = pvm.resolvePath(loc);
        return loc.toString();
    }

    private static Object getFunction(final Object result) {
        if (result instanceof IErlFunctionClause) {
            final IErlFunctionClause clause = (IErlFunctionClause) result;
            final ErlangFunction fc = new ErlangFunction(
                    clause.getFunctionName(), clause.getArity());
            final IErlFunction fun = clause.getModule().findFunction(fc);
            return fun;
        }
        return result;
    }

    public static String getFunctionName(final IErlElement elem) {
        if (elem instanceof IErlFunctionClause) {
            final IErlFunctionClause clause = (IErlFunctionClause) elem;
            return clause.getFunctionName();
        }
        return null;
    }

    protected static IErlModule getModuleFor(IErlElement elem) {
        while (elem != null && !(elem instanceof IErlModule)) {
            elem = (IErlElement) elem.getParent();
        }
        return (IErlModule) elem;
    }

}
