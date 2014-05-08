package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;
import org.junit.Test;

public class ProjectConfigTests {

    @Test
    public void configMapMatchesToolMap() {
        final Map<BuilderTool, Set<ProjectConfigType>> map = BuilderTool.toolConfigsMap;
        final Map<ProjectConfigType, Set<BuilderTool>> inverse = MapUtils.inverseSet(map);
        assertThat(ProjectConfigType.configToolsMap, is(inverse));
    }
}
