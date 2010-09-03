package org.erlide.core.bdd;

import org.jbehave.scenario.JUnitScenario;
import org.jbehave.scenario.MostUsefulConfiguration;
import org.jbehave.scenario.parser.ClasspathScenarioDefiner;
import org.jbehave.scenario.parser.PatternScenarioParser;
import org.jbehave.scenario.parser.ScenarioDefiner;
import org.jbehave.scenario.parser.UnderscoredCamelCaseResolver;
import org.jbehave.scenario.steps.StepsFactory;

public class RpcScenario extends JUnitScenario {

    public RpcScenario() {
        super(new MostUsefulConfiguration() {
            @Override
			public ScenarioDefiner forDefiningScenarios() {
                return new ClasspathScenarioDefiner(new UnderscoredCamelCaseResolver(".scenario"), 
                             new PatternScenarioParser(keywords()));
            }
        });
        //addSteps(new RpcSteps()); // if TraderSteps extends Steps
        addSteps(new StepsFactory().createCandidateSteps(new RpcSteps())); // if TraderSteps is a POJO
    }
}
