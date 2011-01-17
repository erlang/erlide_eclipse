package org.erlide.cover.core;

/**
 * Helper class for defining type of object to perform coverage
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class CoverObject {
    
    public static final int MODULE = 0;
    public static final int DIR = 1;
    
    private int type;
    private String name;
    private String path;
    
    public CoverObject(int type, String path) {
        this(type, "", path);
    }
    
    public CoverObject(int type, String name, String path) {
        this.type = type;
        this.name = name;
        this.path = path;
    }

    public int getType() {
        return type;
    }
    
    public String getName() {
        return name;
    }

    public String getPath() {
        return path;
    }
    
}
