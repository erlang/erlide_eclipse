package org.erlide.ui.internal.compare;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.structuremergeviewer.StructureDiffViewer;
import org.eclipse.swt.widgets.Composite;

public class ErlStructureDiffViewer extends StructureDiffViewer {

	private final ErlStructureCreator fStructureCreator;

	public ErlStructureDiffViewer(final Composite parent,
			final CompareConfiguration config) {
		super(parent, config);
		fStructureCreator = new ErlStructureCreator();
		setStructureCreator(fStructureCreator);
	}
}
