package org.erlide.engine.model.builder;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.Functions;
import org.erlide.engine.model.root.ProjectConfigType;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public enum BuilderTool {
    INTERNAL(null, null), MAKE("Makefile", "make"), EMAKE("Emakefile", null), REBAR(
            "rebar.config", "rebar");

    private final String toolMarker;
    private final String osCommand;

    BuilderTool(final String toolMarker, final String osCommand) {
        this.toolMarker = toolMarker;
        this.osCommand = osCommand;
    }

    /**
     * The marker is the name of a top-level file in the project that tells us
     * to use this tool (used mostly for auto-detection).
     */
    public String getToolMarker() {
        return toolMarker;
    }

    public static final Map<BuilderTool, Set<ProjectConfigType>> toolConfigsMap = new Functions.Function0<Map<BuilderTool, Set<ProjectConfigType>>>() {
        @Override
        public Map<BuilderTool, Set<ProjectConfigType>> apply() {
            final Map<BuilderTool, Set<ProjectConfigType>> result = Maps.newHashMap();
            result.put(INTERNAL, Sets.newHashSet(ProjectConfigType.INTERNAL,
                    ProjectConfigType.EMAKE, ProjectConfigType.REBAR));
            result.put(MAKE, Sets.newHashSet(ProjectConfigType.INTERNAL,
                    ProjectConfigType.EMAKE, ProjectConfigType.REBAR));
            result.put(EMAKE, Sets.newHashSet(ProjectConfigType.EMAKE));
            result.put(REBAR, Sets.newHashSet(ProjectConfigType.REBAR));
            return Maps.newEnumMap(result);
        }
    }.apply();

    /**
     * @return the list of BuilderConfigs that can be used with this tool
     */
    public Collection<ProjectConfigType> getMatchingConfigs() {
        return Collections.unmodifiableCollection(toolConfigsMap.get(this));
    }

    public boolean matchConfig(final ProjectConfigType config) {
        return toolConfigsMap.get(this).contains(config);
    }

    public String getOsCommand() {
        return osCommand;
    }
}
