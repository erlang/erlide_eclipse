package org.erlide.debug.ui.model;

import java.text.MessageFormat;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendListener;
import org.erlide.core.ErlangCore;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.ErlideDebug;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlangDebuggerBackendListener implements IBackendListener {
    @Override
    public void runtimeRemoved(final IBackend backend) {
    }

    @Override
    public void runtimeAdded(final IBackend backend) {
    }

    @Override
    public void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName) {
        try {
            final ErlangDebugTarget erlangDebugTarget = debugTargetOfBackend(backend);
            if (erlangDebugTarget != null
                    && erlangDebugTarget.getInterpretedModules().contains(
                            moduleName)) {
                if (isModuleRunningInInterpreter(erlangDebugTarget, backend,
                        moduleName)) {
                    abortContinueDialog(erlangDebugTarget);
                } else {
                    final ILaunchConfiguration launchConfiguration = erlangDebugTarget
                            .getLaunch().getLaunchConfiguration();
                    final int debugFlags = launchConfiguration.getAttribute(
                            ErlLaunchAttributes.DEBUG_FLAGS,
                            ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
                    final boolean distributed = (debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
                    backend.interpret(project, moduleName, distributed, true);
                }
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    private ErlangDebugTarget debugTargetOfBackend(final IBackend backend) {
        final IDebugTarget[] debugTargets = DebugPlugin.getDefault()
                .getLaunchManager().getDebugTargets();
        for (final IDebugTarget debugTarget : debugTargets) {
            if (debugTarget instanceof ErlangDebugTarget) {
                final ErlangDebugTarget erlangDebugTarget = (ErlangDebugTarget) debugTarget;
                if (erlangDebugTarget.getBackend().equals(backend)) {
                    return erlangDebugTarget;
                }
            }
        }
        return null;
    }

    private void abortContinueDialog(final IDebugTarget target) {
        // do not report errors for snippet editor targets
        // that do not support HCR. HCR is simulated by using
        // a new class loader for each evaluation
        // final ILaunch launch = target.getLaunch();
        // if (launch.getAttribute(ScrapbookLauncher.SCRAPBOOK_LAUNCH) != null)
        // {
        // if (!target.supportsHotCodeReplace()) {
        // return;
        // }
        // }
        final Display display = ErlideUIPlugin.getStandardDisplay();
        if (display.isDisposed()) {
            return;
        }

        String name = null;
        try {
            name = target.getName();
        } catch (final DebugException e) {
            name = ""; // never happens, ErlangDebugTarget doesn't throw this...
        }
        final String vmName = name;
        final ILaunchConfiguration config = target.getLaunch()
                .getLaunchConfiguration();
        final String launchName = config != null ? config.getName()
                : "<unknown>";
        final IStatus status = new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                IStatus.ERROR, "Can't replace code", null);
        final String title = "Code Replace Failed";
        final String message = MessageFormat.format(
                "Some code changes cannot be replaced when being debugged.",
                new Object[] { vmName, launchName });
        display.asyncExec(new Runnable() {
            @Override
            public void run() {
                if (display.isDisposed()) {
                    return;
                }
                final Shell shell = ErlideUIPlugin.getActiveWorkbenchShell();
                final HotCodeReplaceErrorDialog dialog = new HotCodeReplaceErrorDialog(
                        shell, title, message, status, target);
                dialog.setBlockOnOpen(false);
                dialog.open();
            }
        });
    }

    private boolean isModuleRunningInInterpreter(
            final ErlangDebugTarget erlangDebugTarget, final IBackend backend,
            final String moduleName) {
        for (final OtpErlangPid metaPid : erlangDebugTarget.getAllMetaPids()) {
            final List<String> allModulesOnStack = ErlideDebug
                    .getAllModulesOnStack(backend, metaPid);
            if (allModulesOnStack != null) {
                for (final String m : allModulesOnStack) {
                    if (m.equals(moduleName)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

}
