package org.erlide.ui.wizards

import org.erlide.engine.model.root.IErlangProjectProperties
import java.util.Map
import org.erlide.core.internal.builder.BuilderInfo

class ProjectPreferencesWizardPageFactory {

    val static Map<BuilderInfo, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        BuilderInfo.INTERNAL -> InternalProjectPreferencesWizardPage,
        BuilderInfo.MAKE -> MakeProjectPreferencesWizardPage,
        BuilderInfo.EMAKE -> EmakeProjectPreferencesWizardPage,
        BuilderInfo.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(BuilderInfo builder, IErlangProjectProperties info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
