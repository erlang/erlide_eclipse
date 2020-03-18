package org.erlide.ui.wizards;

import java.util.Collections;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;

@SuppressWarnings("all")
public class ProjectPreferencesWizardPageFactory {
    private static final Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> PAGES = Collections
            .<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> unmodifiableMap(
                    CollectionLiterals
                            .<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> newHashMap(
                                    Pair.<ProjectConfigType, Class<InternalProjectPreferencesWizardPage>> of(
                                            ProjectConfigType.INTERNAL,
                                            InternalProjectPreferencesWizardPage.class),
                                    Pair.<ProjectConfigType, Class<EmakeProjectPreferencesWizardPage>> of(
                                            ProjectConfigType.EMAKE,
                                            EmakeProjectPreferencesWizardPage.class),
                                    Pair.<ProjectConfigType, Class<RebarProjectPreferencesWizardPage>> of(
                                            ProjectConfigType.REBAR,
                                            RebarProjectPreferencesWizardPage.class)));

    public static ProjectPreferencesWizardPage create(final ProjectConfigType builder,
            final NewProjectData info) {
        try {
            ProjectPreferencesWizardPage _xblockexpression = null;
            {
                final Class<? extends ProjectPreferencesWizardPage> clazz = ProjectPreferencesWizardPageFactory.PAGES
                        .get(builder);
                final Object _newInstance = clazz.getConstructors()[0]
                        .newInstance("buildPage", info);
                _xblockexpression = (ProjectPreferencesWizardPage) _newInstance;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }
}
