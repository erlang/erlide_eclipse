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
import org.erlide.core.CoreScope;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
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

    public Object execute(ExecutionEvent event) throws ExecutionException {

        InputDialog dialog = new InputDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(),
                "Add user-defined refactoring",
                "Please type callback module name!", "", new IInputValidator() {

                    public IValidator internalV = new ModuleNameValidator();

                    public String isValid(String newText) {
                        if (internalV.isValid(newText))
                            return null;
                        else
                            return "Please type a correct module name!";
                    }
                });

        dialog.open();

        if (dialog.getReturnCode() == Window.CANCEL)
            return null;

        String callbackModule = dialog.getValue();

        RefacType type = checkType(callbackModule);
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

    private void showErrorMesg(String mesg) {
        MessageDialog.openError(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(),
                "Add user-defined refactoring  - error", mesg);
    }

    // check if the refactoring is elementary or composite
    private RefacType checkType(String callbackModule) {

        try {
            IErlModule module = CoreScope.getModel().findModule(callbackModule);
            module.resetAndCacheScannerAndParser(null);

            for (IErlElement el : module.getChildrenOfKind(Kind.ATTRIBUTE)) {
                IErlAttribute attr = (IErlAttribute) el;
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

        } catch (ErlModelException e) {
            return null;
        }
    }

    // look for module path
    private boolean addAndLoad(String callbackModule, RefacType type) {

        String sourcePath = getBinPath(callbackModule);

        IPath destDir = getDestDir(type);
        String destPath = getDestPath(callbackModule, destDir);
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
    private String getBinPath(String callbackModule) {
        String path;

        try {
            if (CoreScope.getModel().findModule(callbackModule) == null)
                return null;

            IErlProject project = CoreScope.getModel()
                    .findModule(callbackModule).getProject();
            path = project.getWorkspaceProject().getLocation()
                    .append(project.getOutputLocation())
                    .append(callbackModule + ".beam").toOSString();

            return path;

        } catch (ErlModelException e) {
            return null;
        }
    }

    // destination directory
    private IPath getDestDir(RefacType type) {
        Bundle coreBundle = Platform.getBundle(Activator.CORE_ID);
        return new Path(coreBundle.getLocation()).append("wrangler")
                .append("ebin").append(type.getDirName());
    }

    // destination path
    private String getDestPath(String callbackModule, IPath dir) {
        String path = dir.append(callbackModule + ".beam").toOSString();
        path = path.substring(path.lastIndexOf(':') + 1);
        return path;
    }

    // copying files
    private boolean copy(String source, String dest, String destDir) {

        File dir = new File(destDir);
        if (!dir.exists()) {
            if (!dir.mkdir())
                return false;
        }

        InputStream in;
        OutputStream out;
        try {
            in = new FileInputStream(source);
        } catch (FileNotFoundException e) {
            return false;
        }
        try {
            out = new FileOutputStream(dest);
        } catch (FileNotFoundException e) {
            return false;
        }

        try {
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        } catch (IOException e) {
            try {
                in.close();
                out.close();
            } catch (IOException ignore) {
            }

            return false;
        }

        return true;
    }

    // invoke loading module
    private void load(String callbackModule, String dir) {
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

        private String dir;

        private RefacType(String dir) {
            this.dir = dir;
        }

        public String getDirName() {
            return dir;
        }
    }

}
