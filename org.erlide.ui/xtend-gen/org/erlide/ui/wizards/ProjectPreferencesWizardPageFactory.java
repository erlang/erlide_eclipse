package org.erlide.ui.wizards;

import com.google.common.collect.Maps;
import java.lang.reflect.Constructor;
import java.util.Collections;
import java.util.Map;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.ui.wizards.EmakeProjectPreferencesWizardPage;
import org.erlide.ui.wizards.InternalProjectPreferencesWizardPage;
import org.erlide.ui.wizards.ProjectPreferencesWizardPage;
import org.erlide.ui.wizards.RebarProjectPreferencesWizardPage;

@SuppressWarnings("all")
public class ProjectPreferencesWizardPageFactory {
  private final static Map<BuilderConfigType,Class<? extends ProjectPreferencesWizardPage>> PAGES = new Function0<Map<BuilderConfigType,Class<? extends ProjectPreferencesWizardPage>>>() {
    public Map<BuilderConfigType,Class<? extends ProjectPreferencesWizardPage>> apply() {
      Map<BuilderConfigType,Class<? extends ProjectPreferencesWizardPage>> _xsetliteral = null;
      Map<BuilderConfigType,Class<? extends ProjectPreferencesWizardPage>> _tempMap = Maps.<BuilderConfigType, Class<? extends ProjectPreferencesWizardPage>>newHashMap();
      _tempMap.put(BuilderConfigType.INTERNAL, InternalProjectPreferencesWizardPage.class);
      _tempMap.put(BuilderConfigType.EMAKE, EmakeProjectPreferencesWizardPage.class);
      _tempMap.put(BuilderConfigType.REBAR, RebarProjectPreferencesWizardPage.class);
      _xsetliteral = Collections.<BuilderConfigType, Class<? extends ProjectPreferencesWizardPage>>unmodifiableMap(_tempMap);
      return _xsetliteral;
    }
  }.apply();
  
  public static ProjectPreferencesWizardPage create(final BuilderConfigType builder, final NewProjectData info) {
    try {
      ProjectPreferencesWizardPage _xblockexpression = null;
      {
        final Class<? extends ProjectPreferencesWizardPage> clazz = ProjectPreferencesWizardPageFactory.PAGES.get(builder);
        Constructor<? extends Object>[] _constructors = clazz.getConstructors();
        Constructor<? extends Object> _get = _constructors[0];
        Object _newInstance = _get.newInstance("buildPage", info);
        _xblockexpression = (((ProjectPreferencesWizardPage) _newInstance));
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
