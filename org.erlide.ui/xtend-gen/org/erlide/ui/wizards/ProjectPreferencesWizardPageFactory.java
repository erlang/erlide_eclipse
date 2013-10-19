package org.erlide.ui.wizards;

import com.google.common.collect.Maps;
import java.lang.reflect.Constructor;
import java.util.Collections;
import java.util.Map;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.core.internal.builder.BuilderInfo;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.ui.wizards.EmakeProjectPreferencesWizardPage;
import org.erlide.ui.wizards.InternalProjectPreferencesWizardPage;
import org.erlide.ui.wizards.MakeProjectPreferencesWizardPage;
import org.erlide.ui.wizards.ProjectPreferencesWizardPage;
import org.erlide.ui.wizards.RebarProjectPreferencesWizardPage;

@SuppressWarnings("all")
public class ProjectPreferencesWizardPageFactory {
  private final static Map<BuilderInfo,Class<? extends ProjectPreferencesWizardPage>> PAGES = new Function0<Map<BuilderInfo,Class<? extends ProjectPreferencesWizardPage>>>() {
    public Map<BuilderInfo,Class<? extends ProjectPreferencesWizardPage>> apply() {
      Map<BuilderInfo,Class<? extends ProjectPreferencesWizardPage>> _xsetliteral = null;
      Map<BuilderInfo,Class<? extends ProjectPreferencesWizardPage>> _tempMap = Maps.<BuilderInfo, Class<? extends ProjectPreferencesWizardPage>>newHashMap();
      _tempMap.put(BuilderInfo.INTERNAL, InternalProjectPreferencesWizardPage.class);
      _tempMap.put(BuilderInfo.MAKE, MakeProjectPreferencesWizardPage.class);
      _tempMap.put(BuilderInfo.EMAKE, EmakeProjectPreferencesWizardPage.class);
      _tempMap.put(BuilderInfo.REBAR, RebarProjectPreferencesWizardPage.class);
      _xsetliteral = Collections.<BuilderInfo, Class<? extends ProjectPreferencesWizardPage>>unmodifiableMap(_tempMap);
      return _xsetliteral;
    }
  }.apply();
  
  public static ProjectPreferencesWizardPage create(final BuilderInfo builder, final IErlangProjectProperties info) {
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
