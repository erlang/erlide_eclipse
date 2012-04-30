/**
 *
 */
package org.erlide.ui.editors.erl.actions;

import java.io.File;
import java.util.ResourceBundle;

import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * @author jakob
 * 
 */
public class ClearCacheAction extends TextEditorAction {

    private final ErlangEditor erlangEditor;
    private static final String NOPARSE_CACHE_SUFFIX = ".noparse";
    private static final String SCANNER_CACHE_SUFFIX = ".scan";
    private static final String suffixes[] = { NOPARSE_CACHE_SUFFIX,
            SCANNER_CACHE_SUFFIX };

    public ClearCacheAction(final ResourceBundle bundle, final String prefix,
            final ErlangEditor erlangEditor) {
        super(bundle, prefix, erlangEditor);
        this.erlangEditor = erlangEditor;
    }

    @Override
    public void run() {
        resetCacheForEditor(erlangEditor);
    }

    public static void resetCacheForEditor(final ErlangEditor erlangEditor) {
        final IErlModule module = erlangEditor.getModule();
        if (module == null) {
            return;
        }
        for (final String suffix : suffixes) {
            final String cacheFileOSPath = ErlangPlugin.getDefault()
                    .getStateLocation()
                    .append(module.getScannerName() + suffix).toOSString();
            final File cacheFile = new File(cacheFileOSPath);
            cacheFile.delete();
        }
        erlangEditor.resetAndCacheScannerAndParser();
    }
}
