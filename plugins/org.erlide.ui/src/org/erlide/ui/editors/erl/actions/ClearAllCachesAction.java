/**
 *
 */
package org.erlide.ui.editors.erl.actions;

import java.io.File;
import java.util.ResourceBundle;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.engine.ErlangEngine;
import org.erlide.ui.editors.erl.ErlangEditor;

// FIXME the Java side should not be aware of the cache details!
public class ClearAllCachesAction extends TextEditorAction {

    public ClearAllCachesAction(final ResourceBundle bundle, final String prefix,
            final ErlangEditor erlangEditor) {
        super(bundle, prefix, erlangEditor);
    }

    @Override
    public void run() {
        ClearAllCachesAction.clearAllCaches();
    }

    public static void clearAllCaches() {
        final String cacheFileOSPath = ErlangEngine.getInstance().getStateDir();
        final File cacheFile = new File(cacheFileOSPath);
        cacheFile.delete();
        for (final IWorkbenchWindow window : PlatformUI.getWorkbench()
                .getWorkbenchWindows()) {
            for (final IWorkbenchPage page : window.getPages()) {
                for (final IEditorReference editor : page.getEditorReferences()) {
                    final IEditorPart ed = editor.getEditor(false);
                    if (ed instanceof ErlangEditor) {
                        ((ErlangEditor) ed).resetAndCacheScannerAndParser();
                    }
                }
            }
        }
    }
}
