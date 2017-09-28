#include "CCredits.h"

#include "NoFlag.xpm"
#include "Canada.xpm"
#include "GreatBritain.xpm"

CCredits::CCredits( QWidget* parent, const char* name )
	: QDialog( parent, name, true )
{
    setSizeGripEnabled( true );
// Qt::WStyle_Customize | Qt::WStyle_NormalBorder /*Qt::WStyle_DialogBorder*/
// Qt::WType_Dialog | Qt::WShowModal

    QGridLayout	*layoutTop  = new QGridLayout( this, 2, 1 );
    layoutTop->setSpacing( 5 );

    QListBox    *list       = new QListBox( this );

    setCaption( "unixODBC - Credits" );

    new QListBoxPixmap( list, QPixmap( xpmGreatBritain ), QString( "Nick Gorham - Current Project Lead, Driver Manager, odbctest, many fixs" ) );
    new QListBoxPixmap( list, QPixmap( xpmCanada ), QString( "Peter Harvey - Original Project Lead, support libs, ODBCConfig, DataManager, isql, odbcinst" ) );
    new QListBoxPixmap( list, QPixmap( xpmCanada ), QString( "Jon Pounder" ) );
    new QListBoxPixmap( list, QPixmap( xpmGreatBritain ), QString( "Martin Evans" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Lars Doelle" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Manush Dodunekov" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Scott Courtney" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Greg Bentz" ) );
    new QListBoxPixmap( list, QPixmap( xpmCanada ), QString( "Shandy J. Brown" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Mark Hessling" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Charles Morrison" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Holger Bischoff" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Charles Overbeck" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Murray Todd Williams" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Jim Ziegler" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Thomas Langen" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Nikolai Afanasiev" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Ralf Fassel" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Tim Roepken" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Zoltan Boszormenyi" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Murad Nayal" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Michael Koch" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Dmitriy Yusupov" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Alex Hornby" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Steve Gilbert" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Max Khon" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Jay Q. Cai" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Bill Bouma" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Steffen Dettmer" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Jens Schlegel" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Venu Anuganti" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Tomas Zellerin" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "James Dugal" ) );
    new QListBoxPixmap( list, QPixmap( xpmNoFlag ), QString( "Simon Pepping" ) );

    layoutTop->addWidget( list, 0, 0 );
    layoutTop->setRowStretch( 0, 10 );

    //
	QGridLayout	*layoutButtons = new QGridLayout( layoutTop, 1, 2 );
    layoutButtons->setSpacing( 5 );
    layoutButtons->setMargin( 5 );
    QPushButton *ppushbuttonOk = new QPushButton( "Ok", this );
    layoutButtons->addWidget( ppushbuttonOk, 0, 1 );
    layoutButtons->setColStretch( 0, 10 );

    connect( ppushbuttonOk, SIGNAL(clicked()), SLOT(accept()) );

    resize( 600, 300 );
}

CCredits::~CCredits()
{
}



