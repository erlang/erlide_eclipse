package org.erlide.ui.wizards

import java.util.Map
import org.erlide.engine.model.builder.BuilderInfo
import org.erlide.engine.model.root.ErlangProjectProperties

class ProjectPreferencesWizardPageFactory {

    val static Map<BuilderInfo, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        BuilderInfo.INTERNAL -> InternalProjectPreferencesWizardPage,
        BuilderInfo.MAKE -> MakeProjectPreferencesWizardPage,
        BuilderInfo.EMAKE -> EmakeProjectPreferencesWizardPage,
        BuilderInfo.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(BuilderInfo builder, ErlangProjectProperties info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
