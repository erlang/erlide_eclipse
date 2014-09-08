package org.erlide.ui.wizards;

import java.lang.reflect.Constructor;
import java.util.Collections;
import java.util.Map;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.wizards.EmakeProjectPreferencesWizardPage;
import org.erlide.ui.wizards.InternalProjectPreferencesWizardPage;
import org.erlide.ui.wizards.ProjectPreferencesWizardPage;
import org.erlide.ui.wizards.RebarProjectPreferencesWizardPage;

@SuppressWarnings("all")
public class ProjectPreferencesWizardPageFactory {
  private final static Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> PAGES = Collections.<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>>unmodifiableMap(CollectionLiterals.<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>>newHashMap(Pair.<ProjectConfigType, Class<InternalProjectPreferencesWizardPage>>of(ProjectConfigType.INTERNAL, InternalProjectPreferencesWizardPage.class), Pair.<ProjectConfigType, Class<EmakeProjectPreferencesWizardPage>>of(ProjectConfigType.EMAKE, EmakeProjectPreferencesWizardPage.class), Pair.<ProjectConfigType, Class<RebarProjectPreferencesWizardPage>>of(ProjectConfigType.REBAR, RebarProjectPreferencesWizardPage.class)));
  
  public static ProjectPreferencesWizardPage create(final ProjectConfigType builder, final NewProjectData info) {
    try {
      ProjectPreferencesWizardPage _xblockexpression = null;
      {
        final Class<? extends ProjectPreferencesWizardPage> clazz = ProjectPreferencesWizardPageFactory.PAGES.get(builder);
        Constructor<?>[] _constructors = clazz.getConstructors();
        Constructor<?> _get = _constructors[0];
        Object _newInstance = _get.newInstance("buildPage", info);
        _xblockexpression = ((ProjectPreferencesWizardPage) _newInstance);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
