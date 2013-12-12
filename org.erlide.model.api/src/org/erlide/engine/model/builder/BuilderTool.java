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

    public static final Map<BuilderTool, Set<BuilderConfigType>> toolConfigsMap = new Functions.Function0<Map<BuilderTool, Set<BuilderConfigType>>>() {
        @Override
        public Map<BuilderTool, Set<BuilderConfigType>> apply() {
            final Map<BuilderTool, Set<BuilderConfigType>> result = Maps.newHashMap();
            result.put(INTERNAL, Sets.newHashSet(BuilderConfigType.INTERNAL,
                    BuilderConfigType.EMAKE, BuilderConfigType.REBAR));
            result.put(MAKE, Sets.newHashSet(BuilderConfigType.INTERNAL, BuilderConfigType.EMAKE,
                    BuilderConfigType.REBAR));
            result.put(EMAKE, Sets.newHashSet(BuilderConfigType.EMAKE));
            result.put(REBAR, Sets.newHashSet(BuilderConfigType.REBAR));
            return Maps.newEnumMap(result);
        }
    }.apply();

    /**
     * @return the list of BuilderConfigs that can be used with this tool
     */
    public Collection<BuilderConfigType> getMatchingConfigs() {
        return Collections.unmodifiableCollection(toolConfigsMap.get(this));
    }

    public boolean matchConfig(final BuilderConfigType config) {
        return toolConfigsMap.get(this).contains(config);
    }

}
