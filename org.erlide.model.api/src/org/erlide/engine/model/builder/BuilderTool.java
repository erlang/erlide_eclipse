package org.erlide.engine.model.builder;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.Functions;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public enum BuilderTool {
    INTERNAL(null), MAKE("Makefile"), EMAKE("Emakefile"), REBAR("rebar.config");

    private final String toolMarker;

    BuilderTool(final String toolMarker) {
        this.toolMarker = toolMarker;
    }

    /**
     * The marker is the name of a top-level file in the project that tells us
     * to use this tool (used mostly for auto-detection).
     */
    public String getToolMarker() {
        return toolMarker;
    }

    public static final Map<BuilderTool, Set<BuilderConfig>> toolConfigsMap = new Functions.Function0<Map<BuilderTool, Set<BuilderConfig>>>() {
        @Override
        public Map<BuilderTool, Set<BuilderConfig>> apply() {
            final Map<BuilderTool, Set<BuilderConfig>> result = Maps.newHashMap();
            result.put(INTERNAL, Sets.newHashSet(BuilderConfig.INTERNAL,
                    BuilderConfig.EMAKE, BuilderConfig.REBAR));
            result.put(MAKE, Sets.newHashSet(BuilderConfig.INTERNAL, BuilderConfig.EMAKE,
                    BuilderConfig.REBAR));
            result.put(EMAKE, Sets.newHashSet(BuilderConfig.EMAKE));
            result.put(REBAR, Sets.newHashSet(BuilderConfig.REBAR));
            return Maps.newEnumMap(result);
        }
    }.apply();

    /**
     * @return the list of BuilderConfigs that can be used with this tool
     */
    public Collection<BuilderConfig> getMatchingConfigs() {
        return Collections.unmodifiableCollection(toolConfigsMap.get(this));
    }

    public boolean matchConfig(final BuilderConfig config) {
        return toolConfigsMap.get(this).contains(config);
    }

}
