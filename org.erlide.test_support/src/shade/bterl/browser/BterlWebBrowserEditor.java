package shade.bterl.browser;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.browser.DefaultBrowserSupport;
import org.eclipse.ui.internal.browser.WebBrowserEditor;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

@SuppressWarnings("restriction")
public class BterlWebBrowserEditor extends WebBrowserEditor {
    public static final String WEB_BROWSER_EDITOR_ID = "org.erlide.test_support.browser"; //$NON-NLS-1$

    @Override
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);
        // need this... why?
        new DefaultBrowserSupport();

        webBrowser.getBrowser().addLocationListener(new LocationListener() {

            public void changing(final LocationEvent event) {
                final IErlFunction ref = getErlangReference(event.location);
                if (ref != null) {
                    openEditor(ref);
                    event.doit = false;
                }
            }

            public void changed(final LocationEvent event) {
                // System.out.println("changed location " + event);
            }
        });
    }

    private void openEditor(final IErlFunction fun) {
        final IWorkbenchPage page = ErlideUIPlugin.getActivePage();

        final IErlModule mod = fun.getModule();

        IEditorInput input = null;
        input = EditorUtility.getEditorInput(mod);
        IEditorPart part = null;
        if (input != null) {
            final String editorId = EditorUtility.getEditorID(input, mod);
            if (editorId != null) {
                try {
                    part = page.openEditor(input, editorId);
                } catch (final PartInitException e) {
                    e.printStackTrace();
                }
            }
        }
        if (part instanceof ErlangEditor) {
            part.setFocus();
            final ErlangEditor ee = (ErlangEditor) part;
            final IDocument d = ee.getDocument();
            int lineStart, lineLength;
            try {
                lineStart = d.getLineOffset(fun.getLineStart());
                lineLength = d.getLineLength(fun.getLineStart());
                EditorUtility.revealInEditor(ee, lineStart, lineLength - 1);
            } catch (final BadLocationException e) {
                e.printStackTrace();
            }
        }
    }

    private IErlFunction getErlangReference(final String location) {
        final IPath path = new Path(location);
        final String refn = path.lastSegment();
        final String[] refs = refn.split("#");
        if (refs.length != 2) {
            return null;
        }

        refs[0] = refs[0].substring(0, refs[0].length() - ".src.html".length());
        final String module = refs[0];
        final String function = refs[1];

        final IErlModule mod = ErlangCore.getModel().findModuleExt(module);
        if (mod == null) {
            return null;
        }

        final IErlFunction fun = mod.findFunction(new ErlangFunction(function));
        if (fun == null) {
            return null;
        }

        return fun;
    }
}
