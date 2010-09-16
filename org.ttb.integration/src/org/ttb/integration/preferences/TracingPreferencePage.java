package org.ttb.integration.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.ttb.integration.Activator;

public class TracingPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    public TracingPreferencePage() {
        super(GRID);
    }

    public void init(IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Set the options for tracing.");
    }

    @Override
    protected void createFieldEditors() {
        StringFieldEditor nodeNameEditor = new StringFieldEditor(PreferenceNames.NODE_NAME, "&Tracing node name:", getFieldEditorParent());
        IntegerFieldEditor netTickTimeEditor = new IntegerFieldEditor(PreferenceNames.TICK_TIME, "&Net tick time (in seconds):", getFieldEditorParent());
        netTickTimeEditor.setValidRange(4, 1000);
        addField(nodeNameEditor);
        addField(netTickTimeEditor);
    }

    public void initializeDefaultPreferences() {
    }
}
