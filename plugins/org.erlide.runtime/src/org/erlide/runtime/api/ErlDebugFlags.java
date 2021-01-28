package org.erlide.runtime.api;

import java.util.EnumSet;

import org.eclipse.jdt.annotation.NonNull;

public enum ErlDebugFlags {

    // @formatter:off
    DISTRIBUTED_DEBUG(1),
    ATTACH_ON_FIRST_CALL(2),
    ATTACH_ON_BREAKPOINT(4),
    ATTACH_ON_EXIT(8);
    // @formatter:on

    public static final EnumSet<@NonNull ErlDebugFlags> DEFAULT_DEBUG_FLAGS = EnumSet
            .of(ErlDebugFlags.ATTACH_ON_BREAKPOINT);

    private final int flag;

    ErlDebugFlags(final int flag) {
        this.flag = flag;
    }

    public int getFlag() {
        return flag;
    }

    public static int getFlag(final EnumSet<@NonNull ErlDebugFlags> set) {
        int result = 0;
        for (final ErlDebugFlags f : set) {
            result |= f.getFlag();
        }
        return result;
    }

    public static EnumSet<@NonNull ErlDebugFlags> makeSet(final int flags) {
        final EnumSet<@NonNull ErlDebugFlags> result = EnumSet
                .noneOf(ErlDebugFlags.class);
        for (final ErlDebugFlags f : ErlDebugFlags.values()) {
            if ((flags & f.getFlag()) != 0) {
                result.add(f);
            }
        }
        return result;
    }
}
