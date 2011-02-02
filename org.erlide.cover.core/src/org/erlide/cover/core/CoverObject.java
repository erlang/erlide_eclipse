package org.erlide.cover.core;

/**
 * Helper class for defining type of object to perform coverage
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverObject {

    public static final int MODULE = 0;
    public static final int PROJ = 1;

    private final int type;
    private final String name;
    private final String pathSrc;
    private final String pathTst;
    private final String pathEbin;

    public CoverObject(final int type, final String pathSrc,
            final String pathTst, final String pathEbin) {
        this(type, "", pathSrc, pathTst, pathEbin);
    }

    public CoverObject(final int type, final String name, final String pathSrc,
            final String pathTst, final String pathEbin) {
        this.type = type;
        this.name = name;
        this.pathSrc = pathSrc;
        this.pathTst = pathTst;
        this.pathEbin = pathEbin;
    }

    public int getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public String getPathSrc() {
        return pathSrc;
    }

    public String getPathTst() {
        return pathTst;
    }

    public String getPathEbin() {
        return pathEbin;
    }

}
