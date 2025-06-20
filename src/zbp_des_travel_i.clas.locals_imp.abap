CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS setTravelId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelId.
    METHODS setOverallStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setOverallStatus.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD setTravelId.

  "Read the entity Travel using EML
  READ ENTITIES OF ZDES_TRAVEL_I IN LOCAL MODE
       ENTITY Travel
       FIELDS ( TravelId )
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_travel).

 " Delete the Record where travel id is already existing
 DELETE lt_travel where travelid is NOT INITIAL.

 SELECT SINGLE from zdes_travel FIELDS MAX( travel_id ) into @DATA(lv_travelid_max).

 " Modify EML
 MODIFY ENTITIES OF ZDES_TRAVEL_I IN LOCAL MODE
       ENTITY Travel
       UPDATE FIELDS ( TravelId )
       with value #( for ls_travel_id in lt_travel INDEX INTO lv_index
                        ( %tky = ls_travel_id-%tky
                          TravelId = lv_travelid_max + lv_index
                           ) ).

  ENDMETHOD.

  METHOD setOverallStatus.

    READ ENTITIES OF ZDES_TRAVEL_I IN LOCAL MODE
     ENTITY Travel
     FIELDS ( OverallStatus )
     WITH CORRESPONDING #( keys )
     RESULT DATA(lt_status).

     DELETE lt_status where OverallStatus is NOT INITIAL.

     MODIFY ENTITIES OF ZDES_TRAVEL_I IN LOCAL MODE
     ENTITY Travel
     UPDATE FIELDS ( OverallStatus )
     WITH VALUE #(  FOR ls_status in lt_status
                   (   %tky = ls_status-%tky
                       OverallStatus = 'O' ) ).








  ENDMETHOD.

ENDCLASS.
