package org.erlide.ui.wizards

import java.util.Map
import org.erlide.engine.model.builder.BuilderConfigType
import org.erlide.engine.model.root.NewProjectData

class ProjectPreferencesWizardPageFactory {

    val static Map<BuilderConfigType, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        BuilderConfigType.INTERNAL -> InternalProjectPreferencesWizardPage,
        BuilderConfigType.EMAKE -> EmakeProjectPreferencesWizardPage,
        BuilderConfigType.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(BuilderConfigType builder, NewProjectData info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
