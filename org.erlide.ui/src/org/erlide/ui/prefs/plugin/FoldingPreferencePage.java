package org.erlide.ui.prefs.plugin;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.erlide.basicui.util.OverlayPreferenceStore;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.FoldingConfigurationBlock;
import org.erlide.ui.prefs.IPreferenceConfigurationBlock;

public final class FoldingPreferencePage extends
		AbstractConfigurationBlockPreferencePage {

	/*
	 * @see org.eclipse.ui.internal.editors.text.AbstractConfigureationBlockPreferencePage#getHelpId()
	 */
	@Override
	protected String getHelpId() {
		return ""; // IJavaHelpContextIds.JAVA_EDITOR_PREFERENCE_PAGE;
	}

	/*
	 * @see org.eclipse.ui.internal.editors.text.AbstractConfigurationBlockPreferencePage#setDescription()
	 */
	@Override
	protected void setDescription() {
		final String description = PreferencesMessages.ErlEditorPreferencePage_folding_title;
		setDescription(description);
	}

	/*
	 * @see org.org.eclipse.ui.internal.editors.text.AbstractConfigurationBlockPreferencePage#setPreferenceStore()
	 */
	@Override
	protected void setPreferenceStore() {
		setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());
	}

	@Override
	protected Label createDescriptionLabel(Composite parent) {
		return null; // no description for new look.
	}

	/*
	 * @see org.eclipse.ui.internal.editors.text.AbstractConfigureationBlockPreferencePage#createConfigurationBlock(org.eclipse.ui.internal.editors.text.OverlayPreferenceStore)
	 */
	@Override
	protected IPreferenceConfigurationBlock createConfigurationBlock(
			OverlayPreferenceStore overlayPreferenceStore) {
		return new FoldingConfigurationBlock(overlayPreferenceStore);
	}
}
