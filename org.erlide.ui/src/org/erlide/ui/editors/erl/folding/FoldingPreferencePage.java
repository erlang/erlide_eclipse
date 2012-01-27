package org.erlide.ui.editors.erl.folding;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.FoldingConfigurationBlock;
import org.erlide.ui.prefs.IPreferenceConfigurationBlock;
import org.erlide.ui.prefs.plugin.PreferencesMessages;
import org.erlide.ui.util.OverlayPreferenceStore;

public final class FoldingPreferencePage extends
        AbstractConfigurationBlockPreferencePage {

    /*
     * @seeorg.eclipse.ui.internal.editors.text.
     * AbstractConfigureationBlockPreferencePage#getHelpId()
     */
    @Override
    protected String getHelpId() {
        return ""; // IErlangHelpContextIds.JAVA_EDITOR_PREFERENCE_PAGE;
    }

    /*
     * @see
     * org.eclipse.ui.internal.editors.text.AbstractConfigurationBlockPreferencePage
     * #setDescription()
     */
    @Override
    protected void setDescription() {
        final String description = PreferencesMessages.ErlEditorPreferencePage_folding_title;
        setDescription(description);
    }

    /*
     * @seeorg.org.eclipse.ui.internal.editors.text.
     * AbstractConfigurationBlockPreferencePage#setPreferenceStore()
     */
    @Override
    protected void setPreferenceStore() {
        setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());
    }

    @Override
    protected Label createDescriptionLabel(final Composite parent) {
        return null; // no description for new look.
    }

    /*
     * @seeorg.eclipse.ui.internal.editors.text.
     * AbstractConfigureationBlockPreferencePage
     * #createConfigurationBlock(org.eclipse
     * .ui.internal.editors.text.OverlayPreferenceStore)
     */
    @Override
    protected IPreferenceConfigurationBlock createConfigurationBlock(
            final OverlayPreferenceStore overlayPreferenceStore) {
        return new FoldingConfigurationBlock(overlayPreferenceStore);
    }
}
