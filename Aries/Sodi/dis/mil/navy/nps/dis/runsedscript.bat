sed -f sedScript template.txt > template.new
sed -f sedScript AcknowledgePdu.java > AcknowledgePdu.new
sed -f sedScript ActionRequestPdu.java > ActionRequestPdu.new
sed -f sedScript ActionResponsePdu.java > ActionResponsePdu.new
sed -f sedScript AngularVelocity.java > AngularVelocity.new
sed -f sedScript ArticulationParameter.java > ArticulationParameter.new
sed -f sedScript BurstDescriptor.java > BurstDescriptor.new
sed -f sedScript ClassUtilities.java > ClassUtilities.new
sed -f sedScript ClockTime.java > ClockTime.new
sed -f sedScript CollisionPdu.java > CollisionPdu.new
sed -f sedScript CommentPdu.java > CommentPdu.new
sed -f sedScript CreateEntityPdu.java > CreateEntityPdu.new
sed -f sedScript DataPdu.java > DataPdu.new
sed -f sedScript DataQueryPdu.java > DataQueryPdu.new
sed -f sedScript DatumSpecification.java > DatumSpecification.new
sed -f sedScript DetonationPdu.java > DetonationPdu.new
sed -f sedScript EntityCoordinate.java > EntityCoordinate.new
sed -f sedScript EntityID.java > EntityID.new
sed -f sedScript EntityStatePdu.java > EntityStatePdu.new
sed -f sedScript EntityType.java > EntityType.new
sed -f sedScript EulerAngle.java > EulerAngle.new
sed -f sedScript EventID.java > EventID.new
sed -f sedScript EventReportPdu.java > EventReportPdu.new
sed -f sedScript FirePdu.java > FirePdu.new
sed -f sedScript FixedDatum.java > FixedDatum.new
sed -f sedScript LinearAcceleration.java > LinearAcceleration.new
sed -f sedScript LinearVelocity.java > LinearVelocity.new
sed -f sedScript NetworkMonitor.java > NetworkMonitor.new
sed -f sedScript PduElement.java > PduElement.new
sed -f sedScript PduTypeEnum.java > PduTypeEnum.new
sed -f sedScript ProtocolDataUnit.java > ProtocolDataUnit.new
sed -f sedScript RemoveEntityPdu.java > RemoveEntityPdu.new
sed -f sedScript SerializationInterface.java > SerializationInterface.new
sed -f sedScript SetDataPdu.java > SetDataPdu.new
sed -f sedScript SimulationManagementPdu.java > SimulationManagementPdu.new
sed -f sedScript StartResumePdu.java > StartResumePdu.new
sed -f sedScript StopFreezePdu.java > StopFreezePdu.new
sed -f sedScript UnsignedByte.java > UnsignedByte.new
sed -f sedScript UnsignedInt.java > UnsignedInt.new
sed -f sedScript UnsignedShort.java > UnsignedShort.new
sed -f sedScript VariableDatum.java > VariableDatum.new
sed -f sedScript WorldCoordinate.java > WorldCoordinate.new

mv template.new template.txt
mv AcknowledgePdu.new AcknowledgePdu.java
mv ActionRequestPdu.new ActionRequestPdu.java
mv ActionResponsePdu.new ActionResponsePdu.java
mv AngularVelocity.new AngularVelocity.java
mv ArticulationParameter.new ArticulationParameter.java
mv BurstDescriptor.new BurstDescriptor.java
mv ClassUtilities.new ClassUtilities.java
mv ClockTime.new ClockTime.java
mv CollisionPdu.new CollisionPdu.java
mv CommentPdu.new CommentPdu.java
mv CreateEntityPdu.new CreateEntityPdu.java
mv DataPdu.new DataPdu.java
mv DataQueryPdu.new DataQueryPdu.java
mv DatumSpecification.new DatumSpecification.java
mv DetonationPdu.new DetonationPdu.java
mv EntityCoordinate.new EntityCoordinate.java
mv EntityID.new EntityID.java
mv EntityStatePdu.new EntityStatePdu.java
mv EntityType.new EntityType.java
mv EulerAngle.new EulerAngle.java
mv EventID.new EventID.java
mv EventReportPdu.new EventReportPdu.java
mv FirePdu.new FirePdu.java
mv FixedDatum.new FixedDatum.java
mv LinearAcceleration.new LinearAcceleration.java
mv LinearVelocity.new LinearVelocity.java
mv NetworkMonitor.new NetworkMonitor.java
mv PduElement.new PduElement.java
mv PduTypeEnum.new PduTypeEnum.java
mv ProtocolDataUnit.new ProtocolDataUnit.java
mv RemoveEntityPdu.new RemoveEntityPdu.java
mv SerializationInterface.new SerializationInterface.java
mv SetDataPdu.new SetDataPdu.java
mv SimulationManagementPdu.new SimulationManagementPdu.java
mv StartResumePdu.new StartResumePdu.java
mv StopFreezePdu.new StopFreezePdu.java
mv UnsignedByte.new UnsignedByte.java
mv UnsignedInt.new UnsignedInt.java
mv UnsignedShort.new UnsignedShort.java
mv VariableDatum.new VariableDatum.java
mv WorldCoordinate.new WorldCoordinate.java

