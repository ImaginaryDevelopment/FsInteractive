-- Generated file, DO NOT edit directly
CREATE TABLE [dbo].[Payment] (
    [PaymentID]                     int             identity primary key,
    [AppointmentId]                 int             null  CONSTRAINT [FK_Payment_AppointmentId_Appointments_AppointmentId] FOREIGN KEY ([AppointmentId]) REFERENCES [dbo].[Appointments] ([AppointmentId]),
    [PaymentTypeId]                 varchar(50)     not null  CONSTRAINT [FK_Payment_PaymentTypeId_PaymentType_PaymentTypeId] FOREIGN KEY ([PaymentTypeId]) REFERENCES [Accounts].[PaymentType] ([PaymentTypeId]), -- |Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod
    [PaymentMethodId]               varchar(50)     not null  CONSTRAINT [FK_Payment_PaymentMethodId_PaymentMethod_PaymentMethodId] FOREIGN KEY ([PaymentMethodId]) REFERENCES [Accounts].[PaymentMethod] ([PaymentMethodId]), -- Cash,CC,Check,Ach,Fsa,Other
    [PaymentStatusId]               varchar(50)     not null  CONSTRAINT [FK_Payment_PaymentStatusId_PaymentStatus_PaymentStatusId] FOREIGN KEY ([PaymentStatusId]) REFERENCES [Accounts].[PaymentStatus] ([PaymentStatusId]), -- New,Partial,Complete
    [TotalAmount]                   decimal(12,2)   not null , -- was Amount (18,2)
    [UserID]                        int             null  CONSTRAINT [FK_Payment_UserID_Users_UserID] FOREIGN KEY ([UserID]) REFERENCES [dbo].[Users] ([UserID]), -- null to allow system inserts/adjustments that aren't done by a user
    [PayerID]                       int             null  CONSTRAINT [FK_Payment_PayerID_Payers_PayerID] FOREIGN KEY ([PayerID]) REFERENCES [dbo].[Payers] ([PayerID]),
    [PatientID]                     int             null  CONSTRAINT [FK_Payment_PatientID_Patients_PatientID] FOREIGN KEY ([PatientID]) REFERENCES [dbo].[Patients] ([PatientID]),
    [Created]                       datetime        null , -- was timestamp
    [TransactionNumber]             varchar(30)     null , -- was checkNumber now will store check number or ACH number (when applicable)
    [Rcd]                           datetime        null , -- Payment Recvd
    [IsElectronic]                  bit             not null ,
    [CCItemID]                      int             null  CONSTRAINT [FK_Payment_CCItemID_CCItem_CCItemID] FOREIGN KEY ([CCItemID]) REFERENCES [Accounts].[CCItem] ([CCItemID]),
    [Comments]                      varchar(MAX)    null 
);

