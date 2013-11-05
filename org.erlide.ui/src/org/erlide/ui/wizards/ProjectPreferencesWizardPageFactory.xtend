package org.erlide.ui.wizards

import java.util.Map
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.builder.BuilderTool

class ProjectPreferencesWizardPageFactory {

    val static Map<BuilderTool, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        BuilderTool.INTERNAL -> InternalProjectPreferencesWizardPage,
        BuilderTool.MAKE -> MakeProjectPreferencesWizardPage,
        BuilderTool.EMAKE -> EmakeProjectPreferencesWizardPage,
        BuilderTool.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(BuilderTool builder, ErlangProjectProperties info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
