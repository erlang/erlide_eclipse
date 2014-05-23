package org.erlide.ui.wizards;

import com.google.common.collect.Maps;
import java.lang.reflect.Constructor;
import java.util.Collections;
import java.util.Map;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.wizards.EmakeProjectPreferencesWizardPage;
import org.erlide.ui.wizards.InternalProjectPreferencesWizardPage;
import org.erlide.ui.wizards.ProjectPreferencesWizardPage;
import org.erlide.ui.wizards.RebarProjectPreferencesWizardPage;

@SuppressWarnings("all")
public class ProjectPreferencesWizardPageFactory {
  private final static Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> PAGES = new Function0<Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>>>() {
    public Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> apply() {
      Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> _xsetliteral = null;
      Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> _tempMap = Maps.<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>>newHashMap();
      _tempMap.put(ProjectConfigType.INTERNAL, InternalProjectPreferencesWizardPage.class);
      _tempMap.put(ProjectConfigType.EMAKE, EmakeProjectPreferencesWizardPage.class);
      _tempMap.put(ProjectConfigType.REBAR, RebarProjectPreferencesWizardPage.class);
      _xsetliteral = Collections.<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>>unmodifiableMap(_tempMap);
      return _xsetliteral;
    }
  }.apply();
  
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
