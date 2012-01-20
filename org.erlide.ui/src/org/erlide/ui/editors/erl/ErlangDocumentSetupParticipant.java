package org.erlide.ui.editors.erl;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.erlide.ui.editors.erl.scanner.ErlangPartitionScanner;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;

public class ErlangDocumentSetupParticipant implements
        IDocumentSetupParticipant {

    @Override
    public void setup(final IDocument document) {
        setupErlangDocumentPartitioner(document,
                IErlangPartitions.ERLANG_PARTITIONING);
    }

    public void setupErlangDocumentPartitioner(final IDocument document,
            final String partitioning) {
        final IDocumentPartitioner partitioner = createDocumentPartitioner();
        if (document instanceof IDocumentExtension3) {
            final IDocumentExtension3 extension3 = (IDocumentExtension3) document;
            extension3.setDocumentPartitioner(partitioning, partitioner);
        } else {
            document.setDocumentPartitioner(partitioner);
        }
        partitioner.connect(document);
    }

    public static IDocumentPartitioner createDocumentPartitioner() {
        return new FastPartitioner(new ErlangPartitionScanner(),
                IErlangPartitions.LEGAL_PARTITIONS);
    }

}
