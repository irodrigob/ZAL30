*&---------------------------------------------------------------------*
*& Report  ZAL30_MAIN_VIEW
*& Versión 1.0.0
*&---------------------------------------------------------------------*
* Description (spanish): Este programa permite la edición de los valores
* de la vista configurada en el programa ZAL30_MAIN_CONF
* Descriptio (english, google translate): This program allows editing
* of values set in the program view ZAL30_MAIN_CONF
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

REPORT  zal30_main_view MESSAGE-ID zca_al30.

INCLUDE zal30_main_view_top.

INCLUDE zal30_main_view_c01. " Verif. datos


*----------------------------------------------------------------------*
* Includes
*----------------------------------------------------------------------*
INCLUDE zal30_main_view_f01.

INCLUDE zal30_main_view_o01.

INCLUDE zal30_main_view_i01.
