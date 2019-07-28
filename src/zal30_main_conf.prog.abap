*&---------------------------------------------------------------------*
*& Report  ZAL30_MAIN_CONF
*& Versión 1.0.0
*&---------------------------------------------------------------------*
* Description (spanish): Este programa es el que permite configurar
* como se verá la tabla/vista en el programa ZAL30_MAIN_VIEW.
* Descriptio (english-google traductor):This program is used to
* configure as you see the table / view in the program ZAL30_MAIN_VIEW.
*&---------------------------------------------------------------------*
*/===================================================================\*
*| ZAL30 is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| ZAL30 is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Iván Rodrigo                                                 |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Iván Rodrigo                                                 |*
*|                                                                   |*
*\===================================================================/*

REPORT  zal30_main_conf MESSAGE-ID zca_al30.

INCLUDE zal30_main_conf_top.

INCLUDE zal30_main_conf_c01. " Verif. pestaña textos

INCLUDE zal30_main_conf_c02. " Evento de toolbar de los campos

START-OF-SELECTION.

*----------------------------------------------------------------------*
* Select data
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM initializacion.

  CALL SCREEN 9000.

*----------------------------------------------------------------------*
* Includes
*----------------------------------------------------------------------*
  INCLUDE zal30_main_conf_f01.

  INCLUDE zal30_main_conf_o01.

  INCLUDE zal30_main_conf_i01.
