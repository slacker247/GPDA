/**************************************************
 * 
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign. 
 * Released under GPL 31.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/
#ifndef CAbout_included
#define CAbout_included

#include <qwidget.h>
#include <qpixmap.h>
#include <qlayout.h>
#include <qmessagebox.h>
#include <qlabel.h>
#include <qpushbt.h>
#include <qframe.h>
#include <qmovie.h>

#include "CCredits.h"


class CAbout : public QWidget
{
    Q_OBJECT

public:

    CAbout( QWidget* parent = NULL, const char* name = NULL );
    virtual ~CAbout();

protected slots:
    void pbODBCConfig_Clicked();
    void pbODBC_Clicked();
    void pbDatabase_Clicked();
    void pbDriverManager_Clicked();
    void pbDriver_Clicked();
    void pbODBCDrivers_Clicked();
    void pbCredits_Clicked();
    void pbApplication_Clicked();

protected:
	QFrame* qtarch_Frame_2;
	QLabel* qtarch_Label_1;

	void resizeEvent( QResizeEvent *p );

};
#endif 
