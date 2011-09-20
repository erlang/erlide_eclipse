package org.erlide.ui.templates;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.persistence.TemplatePersistenceData;
import org.eclipse.jface.text.templates.persistence.TemplateReaderWriter;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlideContributionTemplateStore extends ContributionTemplateStore {

    /**
     * Creates a new template store.
     * 
     * @param store
     *            the preference store in which to store custom templates under
     *            <code>key</code>
     * @param key
     *            the key into <code>store</code> where to store custom
     *            templates
     */
    public ErlideContributionTemplateStore(final IPreferenceStore store,
            final String key) {
        super(store, key);
    }

    /**
     * Creates a new template store with a context type registry. Only templates
     * that specify a context type contained in the registry will be loaded by
     * this store if the registry is not <code>null</code>.
     * 
     * @param registry
     *            a context type registry, or <code>null</code> if all templates
     *            should be loaded
     * @param store
     *            the preference store in which to store custom templates under
     *            <code>key</code>
     * @param key
     *            the key into <code>store</code> where to store custom
     *            templates
     */
    public ErlideContributionTemplateStore(final ContextTypeRegistry registry,
            final IPreferenceStore store, final String key) {
        super(registry, store, key);
    }

    /**
     * Loads the templates contributed via the templates extension point.
     * 
     * @throws IOException
     *             {@inheritDoc}
     */
    @Override
    protected void loadContributedTemplates() throws IOException {
        super.loadContributedTemplates();
        final String erlideTemplates = System.getProperty("erlide.templates");
        if (erlideTemplates != null) {
            final String[] l = erlideTemplates.split(";");
            final Collection<TemplatePersistenceData> templates = new ArrayList<TemplatePersistenceData>();
            readIncludedTemplates(templates, l);
            for (final TemplatePersistenceData data : templates) {
                add(data);
            }
        }
    }

    private void readIncludedTemplates(
            final Collection<TemplatePersistenceData> templates,
            final String[] files) throws IOException {
        for (final String file : files) {
            if (file != null) {
                InputStream stream = null;
                try {
                    final InputStream input = new FileInputStream(
                            new File(file));
                    stream = new BufferedInputStream(input);
                    final TemplateReaderWriter reader = new TemplateReaderWriter();
                    final TemplatePersistenceData[] datas = reader.read(stream,
                            null);
                    for (final TemplatePersistenceData data : datas) {
                        if (validateTemplate(data.getTemplate())) {
                            templates.add(data);
                        }
                    }
                } finally {
                    try {
                        if (stream != null) {
                            stream.close();
                        }
                    } catch (final IOException x) {
                    }
                }
            }
        }
    }

    private boolean validateTemplate(final Template template) {
        final String contextTypeId = template.getContextTypeId();
        if (!contextExists(contextTypeId)) {
            return false;
        }

        if (getRegistry() != null) {
            try {
                getRegistry().getContextType(contextTypeId).validate(
                        template.getPattern());
            } catch (final TemplateException e) {
                ErlideUIPlugin.logErrorMessage("bad formed template: "
                        + template.getName());
                ErlideUIPlugin.log(e);
                return false;
            }
        }
        return true;
    }

    /**
     * Returns <code>true</code> if a context type id specifies a valid context
     * type or if no context type registry is present.
     * 
     * @param contextTypeId
     *            the context type id to look for
     * @return <code>true</code> if the context type specified by the id is
     *         present in the context type registry, or if no registry is
     *         specified
     */
    private boolean contextExists(final String contextTypeId) {
        return contextTypeId != null
                && (getRegistry() == null || getRegistry().getContextType(
                        contextTypeId) != null);
    }
}
