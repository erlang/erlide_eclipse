package org.erlide.runtime.api;

import java.util.EnumSet;

public enum ErlDebugFlags {

    // @formatter:off
    DISTRIBUTED_DEBUG(1),
    ATTACH_ON_FIRST_CALL(2),
    ATTACH_ON_BREAKPOINT(4),
    ATTACH_ON_EXIT(8);
    // @formatter:on

    public static final EnumSet<ErlDebugFlags> DEFAULT_DEBUG_FLAGS = EnumSet
            .of(ATTACH_ON_BREAKPOINT);

    private final int flag;

    private ErlDebugFlags(final int flag) {
        this.flag = flag;
    }

    public int getFlag() {
        return flag;
    }

    public static int getFlag(final EnumSet<ErlDebugFlags> set) {
        int result = 0;
        for (final ErlDebugFlags f : set) {
            result |= f.getFlag();
        }
        return result;
    }

    public static EnumSet<ErlDebugFlags> makeSet(final int flags) {
        final EnumSet<ErlDebugFlags> result = EnumSet.noneOf(ErlDebugFlags.class);
        for (final ErlDebugFlags f : ErlDebugFlags.values()) {
            if ((flags & f.getFlag()) != 0) {
                result.add(f);
            }
        }
        return result;
    }
}
