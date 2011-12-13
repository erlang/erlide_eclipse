package org.erlide.tracing.core.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.tracing.core.Activator;

public class TracingPreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public TracingPreferencePage() {
        super(GRID);
    }

    @Override
    public void init(final IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Set the options for tracing.");
    }

    @Override
    protected void createFieldEditors() {
        final StringFieldEditor nodeNameEditor = new StringFieldEditor(
                PreferenceNames.NODE_NAME, "&Tracing node name:",
                getFieldEditorParent());
        final IntegerFieldEditor netTickTimeEditor = new IntegerFieldEditor(
                PreferenceNames.TICK_TIME, "&Net tick time (in seconds):",
                getFieldEditorParent());
        netTickTimeEditor.setValidRange(4, 1000);
        final IntegerFieldEditor tracesLoadLimitEditor = new IntegerFieldEditor(
                PreferenceNames.TRACES_LOAD_LIMIT,
                "&Max traces in tree viewer:", getFieldEditorParent());
        tracesLoadLimitEditor.setValidRange(1, 150);

        addField(nodeNameEditor);
        addField(netTickTimeEditor);
        addField(tracesLoadLimitEditor);
    }

    public void initializeDefaultPreferences() {
    }
}
