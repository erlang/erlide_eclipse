package shade.bterl.browser;

import java.net.URL;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.browser.InternalBrowserEditorInstance;
import org.eclipse.ui.internal.browser.Messages;
import org.eclipse.ui.internal.browser.Trace;
import org.eclipse.ui.internal.browser.WebBrowserEditorInput;
import org.eclipse.ui.internal.browser.WebBrowserUIPlugin;
import org.erlide.jinterface.util.ErlLogger;

@SuppressWarnings("restriction")
public class BterlBrowserEditorInstance extends InternalBrowserEditorInstance {

    public BterlBrowserEditorInstance(final String id, final int style,
            final String name, final String tooltip) {
        super(id, style, name, tooltip);
    }

    @Override
    public void openURL(final URL url) throws PartInitException {
        if (part != null && !(part instanceof BterlWebBrowserEditor)) {
            ErlLogger.warn("Bterl: browser already hooked (%s)", part
                    .getClass().getName());
            super.openURL(url);
            return;
        }
        final WebBrowserEditorInput input = new WebBrowserEditorInput(url,
                style);
        input.setName(name);
        input.setToolTipText(tooltip);

        final IWorkbenchWindow workbenchWindow = WebBrowserUIPlugin
                .getInstance().getWorkbench().getActiveWorkbenchWindow();
        IWorkbenchPage page = null;
        if (workbenchWindow != null) {
            page = workbenchWindow.getActivePage();
        }

        if (page == null) {
            throw new PartInitException(
                    Messages.errorCouldNotLaunchInternalWebBrowser);
        }

        final BterlWebBrowserEditor editor = (BterlWebBrowserEditor) part;
        if (editor != null) {
            editor.init(editor.getEditorSite(), input);
            page.activate(editor);
        } else {
            try {
                final IEditorPart editorPart = page.openEditor(input,
                        BterlWebBrowserEditor.WEB_BROWSER_EDITOR_ID);
                hookPart(page, editorPart);
            } catch (final Exception e) {
                Trace.trace(Trace.SEVERE, "Error opening Web browser", e); //$NON-NLS-1$
            }
        }
    }

}
