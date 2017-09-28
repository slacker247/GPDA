#include "CStats.h"

#include "info.xpm"

CStats::CStats( QWidget* parent, const char* name )
	: QWidget( parent, name, 0 )
{
	QBoxLayout * pLayoutTop	= new QVBoxLayout( this, 5 );

    // main frame
	QBoxLayout * pLayoutMain = new QHBoxLayout( pLayoutTop, 5 );
    pSummary = new CStatSummary( this );
    pLayoutMain->addWidget( pSummary );
    pDetails = new CStatDetails( this );
    pLayoutMain->addWidget( pDetails );

	// helpt text
	QBoxLayout	*playoutHelp = new QHBoxLayout( pLayoutTop, 5 );

	QFrame* qtarch_Frame_7;
	qtarch_Frame_7 = new QFrame( this, "Frame_7" );
	qtarch_Frame_7->setGeometry( 10, 204, 380, 90 );
	qtarch_Frame_7->setMinimumSize( 380, 90 );
	qtarch_Frame_7->setMaximumSize( 32767, 32767 );
	qtarch_Frame_7->setFrameStyle( QFrame::Box | QFrame::Raised );

	playoutHelp->addWidget( qtarch_Frame_7 );

	QLabel* qtarch_Label_2;
	qtarch_Label_2 = new QLabel( qtarch_Frame_7, "Label_2" );
	qtarch_Label_2->setGeometry( 20, 20, 32, 32 );
	qtarch_Label_2->setPixmap( info_xpm );

	QLabel* qtarch_Label_1;
	qtarch_Label_1 = new QLabel( qtarch_Frame_7, "Label_1" );
	qtarch_Label_1->setGeometry( 70, 10, 310, 70 );
	qtarch_Label_1->setText( "These are the number of active ODBC; environments, connections, statements and descriptors." );
	qtarch_Label_1->setAlignment( AlignLeft | WordBreak );

	resize( 500,330 );
	setMinimumSize( 0, 0 );
	setMaximumSize( 32767, 32767 );
}

CStats::~CStats()
{
}



