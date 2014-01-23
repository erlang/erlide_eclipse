package org.erlide.ui.wizards

import java.util.Map
import org.erlide.engine.model.root.NewProjectData
import org.erlide.engine.model.root.ProjectConfigType

class ProjectPreferencesWizardPageFactory {

    val static Map<ProjectConfigType, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        ProjectConfigType.INTERNAL -> InternalProjectPreferencesWizardPage,
        ProjectConfigType.EMAKE -> EmakeProjectPreferencesWizardPage,
        ProjectConfigType.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(ProjectConfigType builder, NewProjectData info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
