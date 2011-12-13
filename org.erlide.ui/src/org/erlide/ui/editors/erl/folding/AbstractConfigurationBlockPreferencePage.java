package org.erlide.ui.editors.erl.folding;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.IPreferenceConfigurationBlock;
import org.erlide.ui.util.OverlayPreferenceStore;
import org.osgi.service.prefs.BackingStoreException;

abstract class AbstractConfigurationBlockPreferencePage extends PreferencePage
        implements IWorkbenchPreferencePage {

    private final IPreferenceConfigurationBlock fConfigurationBlock;

    private OverlayPreferenceStore fOverlayStore;

    /**
     * Creates a new preference page.
     */
    public AbstractConfigurationBlockPreferencePage() {
        setDescription();
        setPreferenceStore();
        fOverlayStore = new OverlayPreferenceStore(getPreferenceStore(),
                new OverlayPreferenceStore.OverlayKey[] {});
        fConfigurationBlock = createConfigurationBlock(fOverlayStore);
    }

    protected abstract IPreferenceConfigurationBlock createConfigurationBlock(
            OverlayPreferenceStore overlayPreferenceStore);

    protected abstract String getHelpId();

    protected abstract void setDescription();

    protected abstract void setPreferenceStore();

    /*
     * @see IWorkbenchPreferencePage#init()
     */
    @Override
    public void init(final IWorkbench workbench) {
    }

    /*
     * @see PreferencePage#createControl(Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(getControl(), getHelpId());
    }

    /*
     * @see PreferencePage#createContents(Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {

        fOverlayStore.load();
        fOverlayStore.start();

        final Control content = fConfigurationBlock.createControl(parent);

        initialize();

        Dialog.applyDialogFont(content);
        return content;
    }

    private void initialize() {
        fConfigurationBlock.initialize();
    }

    /*
     * @see PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        fConfigurationBlock.performOk();
        fOverlayStore.propagate();
        try {
            ErlideUIPlugin.getPrefsNode().flush();
        } catch (final BackingStoreException e) {
            // ignore
        }
        return true;
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {

        fOverlayStore.loadDefaults();
        fConfigurationBlock.performDefaults();

        super.performDefaults();
    }

    /*
     * @see DialogPage#dispose()
     */
    @Override
    public void dispose() {

        fConfigurationBlock.dispose();

        if (fOverlayStore != null) {
            fOverlayStore.stop();
            fOverlayStore = null;
        }

        super.dispose();
    }
}
