import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Patient } from '../domain/Patient';
import { add } from 'three/webgpu';

@Injectable({
  providedIn: 'root'
})
export class PatientService {
  theServerURL = 'https://localhost:5001/api/Patient';
  httpOptions = {
    headers: new HttpHeaders({'Content-Type': 'application/json' })
  };
  
  constructor(private messageService: MessageService, private http: HttpClient) { }
  
  CreatePatientProfile(firstName: string, lastName: string, phone: string, email: string, address: string, emergencyContact: string, gender: string, dateBirth: string) {
    const url = `${this.theServerURL}/Create-PatientProfile`;
  
  
    let patient: Patient = {
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      datebirth: dateBirth, 
      emergencyContact: emergencyContact,
      gender: gender,
    };
  
    this.http.post<Patient>(url, patient, this.httpOptions).subscribe(data => {
      this.log(`Patient profile: ${data.email} was successfully created.`);
    });
  }

  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
