package org.erlide.ui.wizards

import org.erlide.core.internal.builder.BuildersInfo
import org.erlide.engine.model.root.IErlangProjectProperties
import java.util.Map

class ProjectPreferencesWizardPageFactory {

    val static Map<BuildersInfo, Class<? extends ProjectPreferencesWizardPage>> PAGES = #{
        BuildersInfo.INTERNAL -> InternalProjectPreferencesWizardPage,
        BuildersInfo.MAKE -> MakeProjectPreferencesWizardPage,
        BuildersInfo.EMAKE -> EmakeProjectPreferencesWizardPage,
        BuildersInfo.REBAR -> RebarProjectPreferencesWizardPage
    }

    static def ProjectPreferencesWizardPage create(BuildersInfo builder, IErlangProjectProperties info) {
        val clazz = PAGES.get(builder)
        clazz.constructors.get(0).newInstance("buildPage", info) as ProjectPreferencesWizardPage
    }

}
