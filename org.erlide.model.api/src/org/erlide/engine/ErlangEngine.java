package org.erlide.engine;

/**
 * Facade for the Erlang engine.
 * <p>
 * It serves as a single entry point to all functionality related to handling
 * Erlang code. This way it will be much easier to extract the engine and
 * implement it in Erlang or to let it be used by Xtext.
 * </p>
 */
public class ErlangEngine {
    private volatile static IErlangEngine engine;

    public static synchronized IErlangEngine getInstance() {
        if (engine == null) {
            // TODO inject backend in factory
            engine = ModelActivator.getErlangEngine();
        }
        return engine;
    }
}
