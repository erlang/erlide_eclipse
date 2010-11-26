package org.erlide.core.bdd;

import java.net.URL;
import java.util.List;
import java.util.Properties;

import org.jbehave.core.configuration.Configuration;
import org.jbehave.core.configuration.MostUsefulConfiguration;
import org.jbehave.core.io.CodeLocations;
import org.jbehave.core.io.LoadFromClasspath;
import org.jbehave.core.io.StoryPathResolver;
import org.jbehave.core.io.UnderscoredCamelCaseResolver;
import org.jbehave.core.junit.JUnitStory;
import org.jbehave.core.reporters.StoryReporterBuilder;
import org.jbehave.core.reporters.StoryReporterBuilder.Format;
import org.jbehave.core.steps.CandidateSteps;
import org.jbehave.core.steps.InstanceStepsFactory;
import org.junit.Test;

public abstract class ErlideStory extends JUnitStory {

    public ErlideStory() {
        // start with default configuration, overriding only the elements that
        // are needed
        final StoryPathResolver storyPathResolver = new UnderscoredCamelCaseResolver(
                ".story");
        final Class<?> storyClass = this.getClass();
        final Properties viewProperties = new Properties();
        viewProperties.put("decorateNonHtml", "true");
        final URL codeLocation = CodeLocations
                .codeLocationFromClass(storyClass);
        @SuppressWarnings("unused")
        final StoryReporterBuilder storyReporterBuilder = new StoryReporterBuilder()
                .withCodeLocation(codeLocation)
                .withDefaultFormats()
                .withViewResources(viewProperties)
                .withFormats(Format.CONSOLE, Format.TXT, Format.IDE_CONSOLE,
                        Format.HTML, Format.XML, Format.STATS)
                .withFailureTrace(false);
        final Configuration configuration = new MostUsefulConfiguration()
                .useStoryLoader(
                        new LoadFromClasspath(storyClass.getClassLoader()))
                // .useStoryReporterBuilder(storyReporterBuilder)
                // .useStepMonitor(new SilentStepMonitor())
                .useStoryPathResolver(storyPathResolver);

        useConfiguration(configuration);
        addSteps(createSteps(configuration));
        configuredEmbedder().embedderControls()
                .doGenerateViewAfterStories(true)
                .doIgnoreFailureInStories(false);

    }

    protected List<CandidateSteps> createSteps(final Configuration configuration) {
        return new InstanceStepsFactory(configuration, new RpcSteps())
                .createCandidateSteps();
    }

    @Override
    @Test
    public void run() throws Throwable {
        super.run();
    }

}
