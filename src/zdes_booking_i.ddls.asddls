@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Intf view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@VDM.viewType: #BASIC
define view entity ZDES_BOOKING_I
  as select from zdes_booking
  
  composition [0..*] of ZDES_BKSUPPL_I as _BookingSupplement
  
  association to parent ZDES_TRAVEL_I as _Travel on $projection.TravelUUID = _Travel.TravelUuid
{
  key booking_uuid          as BookingUuid,
      parent_uuid           as TravelUUID,
      booking_id            as BookingId,
      booking_date          as BookingDate,
      customer_id           as CustomerId,
      carrier_id            as CarrierId,
      connection_id         as ConnectionId,
      flight_date           as FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price          as FlightPrice,
      currency_code         as CurrencyCode,
      booking_status        as BookingStatus,
      
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      
      _Travel,
      _BookingSupplement
}
