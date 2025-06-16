@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Consumption Entity'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@VDM.viewType: #CONSUMPTION
define view entity ZDES_BOOKING_C as projection on ZDES_BOOKING_I
{
    key BookingUuid,
    TravelUUID,
    BookingId,
    BookingDate,
    CustomerId,
    CarrierId,
    ConnectionId,
    FlightDate,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    FlightPrice,
    CurrencyCode,
    BookingStatus,
    LocalLastChangedAt,
    /* Associations */
    _BookingSupplement:redirected to composition child ZDES_BKSUPPL_C,
    _Travel:redirected to parent ZDES_TRAVEL_C
}
