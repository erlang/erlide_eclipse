package org.erlide.ui.editors.erl.completion;

import org.eclipse.ui.texteditor.templates.TemplatePreferencePage;
import org.erlide.ui.ErlideUIPlugin;

public class ErlTemplatePreferencePage extends TemplatePreferencePage {

	public ErlTemplatePreferencePage() {
		setPreferenceStore(ErlideUIPlugin.getDefault().getPreferenceStore());
		setTemplateStore(ErlideUIPlugin.getDefault().getTemplateStore());
		setContextTypeRegistry(ErlideUIPlugin.getDefault().getContextTypeRegistry());
	}

	protected boolean isShowFormatterSetting() {
		return false;
	}

	public boolean performOk() {
		boolean ok= super.performOk();

		ErlideUIPlugin.getDefault().savePluginPreferences();

		return ok;
	}

}
