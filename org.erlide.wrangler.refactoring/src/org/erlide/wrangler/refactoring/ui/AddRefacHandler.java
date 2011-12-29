package org.erlide.wrangler.refactoring.ui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlProject;
import org.erlide.wrangler.refactoring.Activator;
import org.erlide.wrangler.refactoring.backend.UserRefactoringsManager;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.ui.validator.ModuleNameValidator;
import org.osgi.framework.Bundle;

/**
 * Handler for adding user-defined refactorings to Wrangler
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class AddRefacHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {

        final InputDialog dialog = new InputDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(),
                "Add user-defined refactoring",
                "Please type callback module name!", "", new IInputValidator() {

                    public IValidator internalV = new ModuleNameValidator();

                    @Override
                    public String isValid(final String newText) {
                        if (internalV.isValid(newText)) {
                            return null;
                        } else {
                            return "Please type a correct module name!";
                        }
                    }
                });

        dialog.open();

        if (dialog.getReturnCode() == Window.CANCEL) {
            return null;
        }

        final String callbackModule = dialog.getValue();

        final RefacType type = checkType(callbackModule);
        if (type == null) {
            showErrorMesg("Callback module must implement either "
                    + "gen_refac or gen_composite_refac behaviour");
            return null;
        }

        if (!addAndLoad(callbackModule, type)) {
            showErrorMesg("Can not load callback module");
            return null;
        } else {
            if (type.equals(RefacType.ELEMENTARY)) {
                UserRefactoringsManager.getInstance().addMyElementary(
                        callbackModule);
            } else {
                UserRefactoringsManager.getInstance().addMyComposite(
                        callbackModule);
            }

            MessageDialog.openInformation(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(),
                    "Add user-defined refactoring", "Success!");
        }

        return null;
    }

    private void showErrorMesg(final String mesg) {
        MessageDialog.openError(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(),
                "Add user-defined refactoring  - error", mesg);
    }

    // check if the refactoring is elementary or composite
    private RefacType checkType(final String callbackModule) {

        try {
            final IErlModule module = ErlModelManager.getErlangModel()
                    .findModule(callbackModule);
            module.resetAndCacheScannerAndParser(null);

            for (final IErlElement el : module
                    .getChildrenOfKind(Kind.ATTRIBUTE)) {
                final IErlAttribute attr = (IErlAttribute) el;
                if (attr.getName().equals("behaviour")
                        || attr.getName().equals("behavior")) {
                    if (attr.getValue().toString().contains("gen_refac")) {
                        return RefacType.ELEMENTARY;
                    } else if (attr.getValue().toString()
                            .contains("gen_composite_refac")) {
                        return RefacType.COMPOSITE;
                    }
                }
            }
            return null;

        } catch (final ErlModelException e) {
            return null;
        }
    }

    // look for module path
    private boolean addAndLoad(final String callbackModule, final RefacType type) {

        final String sourcePath = getBinPath(callbackModule);

        final IPath destDir = getDestDir(type);
        final String destPath = getDestPath(callbackModule, destDir);
        String destDirStr = destDir.toOSString();
        destDirStr = destDirStr.substring(destDirStr.lastIndexOf(":") + 1);

        if (sourcePath == null || destPath == null
                || copy(sourcePath, destPath, destDirStr)) {
            load(callbackModule, destDirStr);
        } else {
            return false;
        }

        return true;
    }

    // gets original binary path
    private String getBinPath(final String callbackModule) {
        String path;

        try {
            if (ErlModelManager.getErlangModel().findModule(callbackModule) == null) {
                return null;
            }

            final IErlProject project = ErlModelManager.getErlangModel()
                    .findModule(callbackModule).getProject();
            path = project.getWorkspaceProject().getLocation()
                    .append(project.getOutputLocation())
                    .append(callbackModule + ".beam").toOSString();

            return path;

        } catch (final ErlModelException e) {
            return null;
        }
    }

    // destination directory
    private IPath getDestDir(final RefacType type) {
        final Bundle coreBundle = Platform.getBundle(Activator.CORE_ID);
        return new Path(coreBundle.getLocation()).append("wrangler")
                .append("ebin").append(type.getDirName());
    }

    // destination path
    private String getDestPath(final String callbackModule, final IPath dir) {
        String path = dir.append(callbackModule + ".beam").toOSString();
        path = path.substring(path.lastIndexOf(':') + 1);
        return path;
    }

    // copying files
    private boolean copy(final String source, final String dest,
            final String destDir) {

        final File dir = new File(destDir);
        if (!dir.exists()) {
            if (!dir.mkdir()) {
                return false;
            }
        }

        InputStream in;
        OutputStream out;
        try {
            in = new FileInputStream(source);
        } catch (final FileNotFoundException e) {
            return false;
        }
        try {
            out = new FileOutputStream(dest);
        } catch (final FileNotFoundException e) {
            return false;
        }

        try {
            final byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        } catch (final IOException e) {
            try {
                in.close();
                out.close();
            } catch (final IOException ignore) {
            }

            return false;
        }

        return true;
    }

    // invoke loading module
    private void load(final String callbackModule, final String dir) {
        WranglerBackendManager.getRefactoringBackend().callWithoutParser(
                "load_callback_mod_eclipse", "ss", callbackModule, dir);
    }

    /**
     * enum for refactoring types - defining refactoring folders
     * 
     * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
     * @version %I%, %G%
     */
    private enum RefacType {
        ELEMENTARY("my_gen_refac"), COMPOSITE("my_gen_composite_refac");

        private final String dir;

        private RefacType(final String dir) {
            this.dir = dir;
        }

        public String getDirName() {
            return dir;
        }
    }

}
