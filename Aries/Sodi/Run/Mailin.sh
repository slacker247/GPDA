#!/bin/sh
#
#echo Test > /tmp/NewMail
cat - > /tmp/NewMail
#mail -f /tmp/NewMail < /tmp/Mailin.in > /tmp/Mailin.out
/home/dre/Sodi/Run/salute /tmp/NewMail
